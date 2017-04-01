import matplotlib.pyplot as plt
import tensorflow as tf
import numpy as np
from IPython.display import YouTubeVideo


video_lvl_record = "data/video_level/train/train-0.tfrecord"

# stores the video id - String
vid_ids = []    # [1170]


# Stores the labels associated with each video.
# Each entry is a list of variable size - Numeric
labels = []             # [1170 x ?]
one_hot_labels = [[] for i in range(4716)]     # [1170 x 4716]

# Stores label = [video-ids]
label_id_map = {}

###    RGB    ###
# stores the mean RGB valueS of each videos for 
# each frame(1024). Each entry is a list of size 1024,
# where each entry is the mean RGB for that
# particular frame.
mean_rgb = []   # [1170 x 1024]

# stores the mean RGB value of each video for ALL
# the frames. Each entry is a value
avg_rgb = []    # [1170 x 1]


###    AUDIO    ###
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

print('Number of videos in this tfrecord: ', len(mean_rgb))
print("\nLabels of video-0:", labels[0])
print("\nID of video-0:", vid_ids[0])


#print('\nIDs with label 1', label_id_map[1])


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

cost = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits=py_x, labels=Y))
train_op = tf.train.RMSPropOptimizer(0.001, 0.9).minimize(cost)
predict_op = tf.argmax(py_x, 1)

trX = mean_rgb
trY = one_hot_labels
batch_size = 10
# Launch the graph in a session
with tf.Session() as sess:
    # you need to initialize all variables
    tf.global_variables_initializer().run()

    for i in range(10):
        print("Round#",i)
        for start, end in zip(range(0, len(trX), batch_size), range(batch_size, len(trX) + 1, batch_size)):
            sess.run(train_op, feed_dict={X: trX[start:end], Y: trY[start:end], p_keep_input: 0.8, p_keep_hidden: 0.5})
        
        y_0 = sess.run(py_x, feed_dict={X: trX[0:1], p_keep_input: 1, p_keep_hidden: 1})
        #if(DEBUG): print("output of output layer (without softmax) : " , y_0)
        
        y_0_soft = sess.run(tf.nn.softmax(y_0))
        #if(DEBUG): print("softmax : ", y_0_soft)
        
        tags,indices = sess.run(tf.nn.top_k(y_0_soft, 2))
        if(DEBUG): print(tags, indices)
            
        #predictions_vector = sess.run(predict_op, feed_dict={X: teX, p_keep_input: 1.0, p_keep_hidden: 1.0})
        #print("iteration :", i, " accuracy :", np.mean(np.argmax(teY, axis=1) == predictions_vector))
    


