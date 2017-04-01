import tensorflow as tf
import numpy as np
from os import listdir
from os.path import isfile, join

label_id_map = {}
train_path = "data/video_level/train/"
train_files = [join(train_path,f) for f in listdir(train_path) if isfile(join(train_path, f))]

### Template for traversing files ###
def readTFRecord(train_file):
    video_lvl_record = train_file
    
    vid_ids = []        # [1170]
    labels = []         # [1170 x ?]
    one_hot_labels = [[] for i in range(4716)]     # [1170 x 4716]

    mean_rgb = []   # [1170 x 1024]
    avg_rgb = []    # [1170 x 1]
    
    mean_audio = [] # [1170 x 128] 
    avg_audio = []  # [1170 x 1]
    
    i=0
    for example in tf.python_io.tf_record_iterator(video_lvl_record):
        tf_example = tf.train.Example.FromString(example)
        
        vid_ids.append(tf_example.features.feature['video_id'].bytes_list.value[0].decode(encoding='UTF-8'))
        labels.append(tf_example.features.feature['labels'].int64_list.value)
        
        mean_rgb.append(tf_example.features.feature['mean_rgb'].float_list.value)
        avg_rgb.append(np.mean(tf_example.features.feature['mean_rgb'].float_list.value))
        
        mean_audio.append(tf_example.features.feature['mean_audio'].float_list.value)
        avg_audio.append(np.mean(tf_example.features.feature['mean_audio'].float_list.value))
    
        current_labels = labels[i]
        current_id = vid_ids[i]
        one_hot = [0] * 4716
        for label in current_labels:
            if label in label_id_map:
                label_id_map[label].append((current_id,i))
            else:
                label_id_map[label]=[(current_id,i)]
            one_hot[label] = 1
        one_hot_labels[i] = one_hot
        
        i+=1
    
    print('\nNumber of videos in',train_file,': ', len(mean_rgb))
    print("Labels of video-0:", np.array(labels[0]))
    
    return mean_rgb, one_hot_labels



### Statistics Related Functions ###
# stores <id, count>
class_statistics = [0] * 4716
def getClassStats(train_file):
    video_lvl_record = train_file
    
    labels = []         # [1170 x ?]
    
    i=0
    for example in tf.python_io.tf_record_iterator(video_lvl_record):
        tf_example = tf.train.Example.FromString(example)
        
        labels.append(tf_example.features.feature['labels'].int64_list.value)
        
        current_labels = labels[i]
        for label in current_labels:
            class_statistics[label] = class_statistics[label] + 1
        
        i+=1
    
    print("Labels of video-0:", np.array(labels[0]))

file_no = 0
for file in train_files:
    print("Reading file#",file_no)
    getClassStats(file)
    file_no = file_no + 1

total_videos = sum(class_statistics)
class_stat_percent = [count/total_videos for count in class_statistics]
class_stat_minus = [1 - avg for avg in class_stat_percent]
class_stat_balance = [1 + 4*avg for avg in class_stat_minus]



### RGB Relates Functions ###
def getMeanRGBAndLabels(train_file):
    video_lvl_record = train_file
    
    vid_ids = []        # [1170]
    labels = []         # [1170 x ?]
    one_hot_labels = [[] for i in range(4716)]     # [1170 x 4716]

    mean_rgb = []   # [1170 x 1024]
    avg_rgb = []    # [1170 x 1]
    
    i=0
    for example in tf.python_io.tf_record_iterator(video_lvl_record):
        tf_example = tf.train.Example.FromString(example)
        
        vid_ids.append(tf_example.features.feature['video_id'].bytes_list.value[0].decode(encoding='UTF-8'))
        labels.append(tf_example.features.feature['labels'].int64_list.value)
        
        mean_rgb.append(tf_example.features.feature['mean_rgb'].float_list.value)
        avg_rgb.append(np.mean(tf_example.features.feature['mean_rgb'].float_list.value))
        
        current_labels = labels[i]
        current_id = vid_ids[i]
        one_hot = [0] * 4716
        for label in current_labels:
            if label in label_id_map:
                label_id_map[label].append((current_id,i))
            else:
                label_id_map[label]=[(current_id,i)]
            one_hot[label] = 1
        one_hot_labels[i] = one_hot
        
        i+=1
    
    print('\nNumber of videos in',train_file,': ', len(mean_rgb))
    print("Labels of video-0:", np.array(labels[0]))
    
    return mean_rgb, one_hot_labels



### 2 Hidden Layer NN ### 
DEBUG = True
def init_weights(shape):
    return tf.Variable(tf.random_normal(shape, stddev=1))


def model(X, w_h, w_h2, w_o, p_keep_input, p_keep_hidden):
    X = tf.nn.dropout(X, p_keep_input)
    h = tf.nn.relu(tf.matmul(X, w_h))

    h = tf.nn.dropout(h, p_keep_hidden)
    h2 = tf.nn.relu(tf.matmul(h, w_h2))

    h2 = tf.nn.dropout(h2, p_keep_hidden)

    return tf.matmul(h2, w_o)

# size of layers
input_size = 1024       # mean RGB of each frame(1024)
n_hidden1_units = 100
n_hidden2_units = 50
output_size = 4716      # no of labels

X = tf.placeholder("float", [None, 1024])
Y = tf.placeholder("float", [None, 4716])

weight_hidden_1 = init_weights([input_size, n_hidden1_units])
weight_hidden_2 = init_weights([n_hidden1_units, n_hidden2_units])
weight_output = init_weights([n_hidden2_units, output_size])

p_keep_input = tf.placeholder("float")
p_keep_hidden = tf.placeholder("float")
py_x = model(X, weight_hidden_1, weight_hidden_2, weight_output, p_keep_input, p_keep_hidden)

cost = tf.reduce_mean(tf.nn.sigmoid_cross_entropy_with_logits(logits=py_x, labels=Y))
train_op = tf.train.RMSPropOptimizer(0.001, 0.9).minimize(cost)
predict_op = tf.argmax(py_x, 1)

batch_size = 10
with tf.Session() as sess:
    tf.global_variables_initializer().run()

    for file in train_files:
        trX, trY = getMeanRGBAndLabels(file)
        for i in range(10):
            #print("Round#",i)
            for start, end in zip(range(0, len(trX), batch_size), range(batch_size, len(trX) + 1, batch_size)):
                trY_star = trY[start:end]
                '''
                for video_index, videos_labels in enumerate(trY_star):
                    for label_index, video_labels in enumerate(trY_star[video_index]):
                        video_label = trY_star[video_index][label_index]
                        
                        try:
                            if video_label == 1:
                                trY_star[video_index][label_index] = class_stat_balance[label_index]
                        except TypeError:
                            print(video_index, label_index, video_label)
                '''
                sess.run(train_op, feed_dict={X: trX[start:end], Y: trY_star, p_keep_input: 0.8, p_keep_hidden: 0.5})
        
        y_0 = sess.run(py_x, feed_dict={X: trX[0:1], p_keep_input: 1, p_keep_hidden: 1})
        #if(DEBUG): print("output of output layer (without softmax) : " , y_0)
            
        y_0_soft = sess.run(tf.nn.sigmoid(y_0))
        #if(DEBUG): print("softmax : ", y_0_soft)
            
        tags,indices = sess.run(tf.nn.top_k(y_0_soft, 5))
        if(DEBUG): print(tags, indices)
            
            #predictions_vector = sess.run(predict_op, feed_dict={X: teX, p_keep_input: 1.0, p_keep_hidden: 1.0})
            #print("iteration :", i, " accuracy :", np.mean(np.argmax(teY, axis=1) == predictions_vector))
    


