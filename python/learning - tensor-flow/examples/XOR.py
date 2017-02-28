import tensorflow as tf
import numpy as np
from random import randint

DEBUG=True

def init_weights(shape):
    return tf.Variable(tf.random_normal(shape, stddev=0.01))


def model(X, weight_hidden, weight_output):
    # [1,3] x [3,n_hiddent_units] = [1,n_hiddent_units]
    hiddern_units_output = tf.nn.sigmoid(tf.matmul(X, weight_hidden))
    
    # [1,n_hiddent_units] x [n_hiddent_units, 2] = [1,1]
    return tf.nn.sigmoid(tf.matmul(hiddern_units_output, weight_output))


def getHiddenLayerOutput(X, weight_hidden):
    hiddern_units_output = tf.nn.sigmoid(tf.matmul(X, weight_hidden))
    return hiddern_units_output

total_inputs = 1000
zeros = tf.zeros([total_inputs,1])
ones = tf.ones([total_inputs,1])
around_zeros = tf.random_normal([total_inputs,1], mean=0, stddev=0.01)
around_ones = tf.random_normal([total_inputs,1], mean=1, stddev=0.01)

batch_size = 100
n_hiddent_units = 2
X = tf.placeholder("float", [None, 3])
Y = tf.placeholder("float", [None, 1])

weight_hidden = init_weights([3, n_hiddent_units])
weight_output = init_weights([n_hiddent_units, 1])

hiddern_units_output = getHiddenLayerOutput(X, weight_hidden)
py_x = model(X, weight_hidden, weight_output)

#cost = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits=py_x, labels=Y))
cost = tf.square(Y - py_x) 
train_op = tf.train.GradientDescentOptimizer(0.05).minimize(cost)
#predict_op = tf.argmax(py_x, 1)

with tf.Session() as sess:
    # you need to initialize all variables
    tf.global_variables_initializer().run()
    
    trX_0_0 = sess.run(tf.concat([ones, around_zeros, around_zeros, zeros], axis=1))
    trX_0_1 = sess.run(tf.concat([ones, around_zeros, around_ones, ones], axis=1))
    trX_1_0 = sess.run(tf.concat([ones, around_ones, around_zeros, ones], axis=1))
    trX_1_1 = sess.run(tf.concat([ones, around_ones, around_ones, zeros], axis=1))
    trX = sess.run(tf.concat([trX_0_0, trX_0_1, trX_1_0, trX_1_1], axis=0))
    trX = sess.run(tf.random_shuffle(trX))
    print(trX)
    
    for i in range(10):
        for start, end in zip(range(0, len(trX), batch_size), range(batch_size, len(trX) + 1, batch_size)):
            trY = tf.identity(trX[start:end,3])
            trY = sess.run(tf.reshape(trY,[batch_size, 1]))
            sess.run(train_op, feed_dict={ X: trX[start:end,0:3], Y: trY })
            
        #if(DEBUG): print("weights for hidden : ", sess.run(weight_hidden))
        start_index = randint(0, (total_inputs*4)-batch_size)
        #print(start_index)
        
        #h_0 = sess.run(hiddern_units_output, feed_dict={X: trX[start_index: start_index+batch_size]})
        #if(DEBUG): print("hidden layer output :",h_0)

        y_0 = sess.run(py_x, feed_dict={X: trX[start_index:start_index+batch_size,0:3]})
        #if(DEBUG): print("output of output layer : " , y_0)
        
        print("iteration :",i, " accuracy :", np.mean(np.absolute(trX[start_index:start_index+batch_size,3]-y_0)),"\n")
