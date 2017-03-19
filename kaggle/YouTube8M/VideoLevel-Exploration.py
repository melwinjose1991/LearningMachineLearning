import matplotlib.pyplot as plt
import tensorflow as tf
import numpy as np
from IPython.display import YouTubeVideo


video_lvl_record = "data/video_level/train/train-0.tfrecord"

vid_ids = []
labels = []

mean_rgb = []
mean_audio = []

avg_rgb = []
avg_audio = []

for example in tf.python_io.tf_record_iterator(video_lvl_record):
    tf_example = tf.train.Example.FromString(example)
    
    vid_ids.append(tf_example.features.feature['video_id'].bytes_list.value[0].decode(encoding='UTF-8'))
    labels.append(tf_example.features.feature['labels'].int64_list.value)
    
    mean_rgb.append(tf_example.features.feature['mean_rgb'].float_list.value)
    avg_rgb.append(np.mean(tf_example.features.feature['mean_rgb'].float_list.value))
    
    mean_audio.append(tf_example.features.feature['mean_audio'].float_list.value)
    avg_audio.append(np.mean(tf_example.features.feature['mean_audio'].float_list.value))

print('Number of videos in this tfrecord: ', len(mean_rgb))
print("\nOverAll Average RGB:", avg_rgb[0])
print('\nOverAll Average Audio', avg_audio[0])


def getLabel(all_labels, category):
    new_labels = []
    for labels in all_labels:
        if category in labels:
            new_labels.append(1)
        else:
            new_labels.append(0)
    return new_labels

def getFirst(all_labels):
    new_labels = []
    for labels in all_labels:
        new_labels.append(labels[0])
    return new_labels

plt.scatter(avg_rgb, getLabel(labels, 1))

