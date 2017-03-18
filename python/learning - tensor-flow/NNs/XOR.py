import tensorflow as tf
import numpy as np
from random import randint

DEBUG = True

total_inputs = 1000
batch_size = 10
n_hiddent_units = 2

ones_for_layer3 = tf.ones([batch_size, 1])

def init_weights(shape):
    return tf.Variable(tf.random_normal(shape, stddev=1))


def model(X, weight_hidden, weight_output):
    # [1,3] x [3,n_hiddent_units] = [1,n_hiddent_units]
    layer2_output = tf.nn.sigmoid(tf.matmul(X, weight_hidden))
    
    layer3_input = tf.concat([layer2_output, ones_for_layer3], axis=1)
    
    # [1,n_hiddent_units+1] x [1+n_hiddent_units, 2] = [1,2]
    return tf.matmul(layer3_input, weight_output)
    # return hiddern_units_output


def getHiddenLayerOutput(X, weight_hidden):
    hidden_units_output = tf.nn.sigmoid(tf.matmul(X, weight_hidden))
    return hidden_units_output


zeros = tf.zeros([total_inputs, 1])
ones = tf.ones([total_inputs, 1])
around_zeros = tf.random_normal([total_inputs, 1], mean=0, stddev=0.01)
around_ones = tf.random_normal([total_inputs, 1], mean=1, stddev=0.01)

X = tf.placeholder("float", [None, 3])
Y = tf.placeholder("float", [None, 2])

weight_hidden = init_weights([3, n_hiddent_units])
weight_output = init_weights([n_hiddent_units + 1, 2])

hiddern_units_output = getHiddenLayerOutput(X, weight_hidden)
py_x = model(X, weight_hidden, weight_output)

cost = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits=py_x, labels=Y)) 
train_op = tf.train.GradientDescentOptimizer(0.05).minimize(cost)
predict_op = tf.argmax(py_x, 1)

with tf.Session() as sess:
    tf.global_variables_initializer().run()
    
    trX_0_0 = sess.run(tf.concat([ones, zeros, zeros, ones, zeros], axis=1))
    trX_0_1 = sess.run(tf.concat([ones, zeros, ones, zeros, ones], axis=1))
    trX_1_0 = sess.run(tf.concat([ones, ones, zeros, zeros, ones], axis=1))
    trX_1_1 = sess.run(tf.concat([ones, ones, ones, ones, zeros], axis=1))
    trX = sess.run(tf.concat([trX_0_0, trX_0_1, trX_1_0, trX_1_1], axis=0))
    trX = sess.run(tf.random_shuffle(trX))
    print(trX)
    
    trY = sess.run(tf.reshape(tf.identity(trX[0:total_inputs * 4, 3:5]), [total_inputs * 4, 2]))
    
    for i in range(15):
        for start, end in zip(range(0, len(trX), batch_size), range(batch_size, len(trX) + 1, batch_size)):
            sess.run(train_op, feed_dict={ X: trX[start:end, 0:3], Y: trY[start:end] })
           
        start_index = randint(0, (total_inputs * 4) - batch_size)
        
        predicted_output = sess.run(predict_op, feed_dict={ X: trX[start_index:start_index + batch_size, 0:3] })
        #print(predicted_output)
        
        actual_output = sess.run(tf.argmax(trX[start_index:start_index + batch_size, 3:5], 1))
        #print(actual_output)
    
        print("iteration :", i, " accuracy :", np.mean(actual_output == predicted_output), "\n")
    
    print("trained weight for hidden layer\n", sess.run(weight_hidden))
    print("\ntrained weights for output layer\n", sess.run(weight_output))
