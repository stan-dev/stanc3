
import numpy as np__
import tensorflow as tf__
import tensorflow_probability as tfp__
tfd__ = tfp__.distributions
tfb__ = tfp__.bijectors
from tensorflow.python.ops.parallel_for import pfor as pfor__

class test_cont_01_model(tfd__.Distribution):

  def __init__(self):
    pass
     
  
  def log_prob_one_chain(self, params):
    target = 0
    
    p = tf__.cast(params[0], tf__.float64)
    target += tf__.reduce_sum(tfd__.Beta(tf__.cast(1.5, tf__.float64),
                                         tf__.cast(0.8, tf__.float64)).log_prob(p))
    return target
     
  def log_prob(self, params):
    return tf__.vectorized_map(self.log_prob_one_chain, params)
    
     
  def parameter_shapes(self, nchains__):
    
    return [(nchains__, )]
     
  def parameter_bijectors(self):
    
    return [tfb__.Chain([tfb__.Shift(tf__.cast(0, tf__.float64)),
                         tfb__.Scale(tf__.cast(1, tf__.float64) - tf__.cast(0, tf__.float64)),
                         tfb__.Sigmoid()])]
     
  def parameter_names(self):
    return ["p"]
     
model = test_cont_01_model