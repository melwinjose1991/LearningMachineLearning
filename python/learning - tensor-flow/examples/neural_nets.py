# source : https://github.com/nlintz/TensorFlow-Tutorials/blob/master/03_net.py

import tensorflow as tf
import numpy as np
from tensorflow.examples.tutorials.mnist import input_data

DEBUG=False

def init_weights(shape):
    '''
    tf.random_normal(shape, mean=0.0, stddev=1.0)
        Outputs random values from a normal distribution.
        shape: A 1-D integer Tensor or Python array. The shape of the output tensor.
    '''
    return tf.Variable(tf.random_normal(shape, stddev=0.01))


def model(X, weight_hidden, weight_output):
    # [1,784] x [784,n_hiddent_units] = [1,n_hiddent_units]
    hiddern_units_output = tf.nn.sigmoid(tf.matmul(X, weight_hidden))
    
    # [1,n_hiddent_units] x [n_hiddent_units, 10] = [1,10]
    return tf.matmul(hiddern_units_output, weight_output)


def getHiddenLayerOutput(X, weight_hidden):
    hiddern_units_output = tf.nn.sigmoid(tf.matmul(X, weight_hidden))
    return hiddern_units_output


mnist = input_data.read_data_sets("MNIST_data/", one_hot=True)
trX, trY, teX, teY = mnist.train.images, mnist.train.labels, mnist.test.images, mnist.test.labels

n_hiddent_units = 25
X = tf.placeholder("float", [None, 784])
Y = tf.placeholder("float", [None, 10])

# input nodes 784
# hidden nodes n_hiddent_units
# output nodes 10
weight_hidden = init_weights([784, n_hiddent_units])
weight_output = init_weights([n_hiddent_units, 10])

hiddern_units_output = getHiddenLayerOutput(X, weight_hidden)
py_x = model(X, weight_hidden, weight_output)

# softmax_cross_entropy_with_logits : NN for ML 4.c
cost = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits=py_x, labels=Y))
train_op = tf.train.GradientDescentOptimizer(0.05).minimize(cost)
predict_op = tf.argmax(py_x, 1)

# Launch the graph in a session
with tf.Session() as sess:
    # you need to initialize all variables
    tf.global_variables_initializer().run()

    for i in range(10):
        for start, end in zip(range(0, len(trX), 128), range(128, len(trX) + 1, 128)):
            sess.run(train_op, feed_dict={X: trX[start:end], Y: trY[start:end]})

        h_0 = sess.run(hiddern_units_output, feed_dict={X: trX[0:1]})
        if(DEBUG): print("hidden layer output :",h_0)

        y_0 = sess.run(py_x, feed_dict={X: trX[0:1]})
        if(DEBUG): print("output of output layer (without softmax) : " , y_0)
        
        y_0_soft = sess.run(tf.nn.softmax(y_0))
        if(DEBUG): print("softmax : ", y_0_soft)
        
        predictions_vector = sess.run(predict_op, feed_dict={X: teX})
        print("iteration :",i, " accuracy :", np.mean(np.argmax(teY, axis=1) == predictions_vector))
