# source : https://github.com/nlintz/TensorFlow-Tutorials/blob/master/04_modern_net.py

import tensorflow as tf
import numpy as np
from tensorflow.examples.tutorials.mnist import input_data

DEBUG = False

def init_weights(shape):
    '''
    tf.random_normal(shape, mean=0.0, stddev=1.0)
        Outputs random values from a normal distribution.
        shape: A 1-D integer Tensor or Python array. The shape of the output tensor.
    '''
    return tf.Variable(tf.random_normal(shape, stddev=0.01))


def model(X, w_h, w_h2, w_o, p_keep_input, p_keep_hidden):
    X = tf.nn.dropout(X, p_keep_input)
    h = tf.nn.relu(tf.matmul(X, w_h))

    h = tf.nn.dropout(h, p_keep_hidden)
    h2 = tf.nn.relu(tf.matmul(h, w_h2))

    h2 = tf.nn.dropout(h2, p_keep_hidden)

    return tf.matmul(h2, w_o)

mnist = input_data.read_data_sets("MNIST_data/", one_hot=True)
trX, trY, teX, teY = mnist.train.images, mnist.train.labels, mnist.test.images, mnist.test.labels

n_hidden1_units = 625
n_hidden2_units = 625
X = tf.placeholder("float", [None, 784])
Y = tf.placeholder("float", [None, 10])

# input nodes 784
# hidden nodes n_hiddent_units
# output nodes 10
weight_hidden_1 = init_weights([784, n_hidden1_units])
weight_hidden_2 = init_weights([n_hidden1_units, n_hidden2_units])
weight_output = init_weights([n_hidden2_units, 10])

p_keep_input = tf.placeholder("float")
p_keep_hidden = tf.placeholder("float")
py_x = model(X, weight_hidden_1, weight_hidden_2, weight_output, p_keep_input, p_keep_hidden)

cost = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits=py_x, labels=Y))
train_op = tf.train.RMSPropOptimizer(0.001, 0.9).minimize(cost)
predict_op = tf.argmax(py_x, 1)

# Launch the graph in a session
with tf.Session() as sess:
    # you need to initialize all variables
    tf.global_variables_initializer().run()

    for i in range(10):
        for start, end in zip(range(0, len(trX), 128), range(128, len(trX) + 1, 128)):
            sess.run(train_op, feed_dict={X: trX[start:end], Y: trY[start:end], p_keep_input: 0.8, p_keep_hidden: 0.5})
        
        '''
        h_0 = sess.run(hiddern_units_output, feed_dict={X: trX[0:1]})
        if(DEBUG): print("hidden layer output :", h_0)

        y_0 = sess.run(py_x, feed_dict={X: trX[0:1]})
        if(DEBUG): print("output of output layer (without softmax) : " , y_0)
        
        y_0_soft = sess.run(tf.nn.softmax(y_0))
        if(DEBUG): print("softmax : ", y_0_soft)
        '''
            
        predictions_vector = sess.run(predict_op, feed_dict={X: teX, p_keep_input: 1.0, p_keep_hidden: 1.0})
        print("iteration :", i, " accuracy :", np.mean(np.argmax(teY, axis=1) == predictions_vector))
