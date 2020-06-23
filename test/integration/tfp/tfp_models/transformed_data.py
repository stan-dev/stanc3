
import numpy as np__
import tensorflow as tf__
import tensorflow_probability as tfp__
tfd__ = tfp__.distributions
tfb__ = tfp__.bijectors
from tensorflow.python.ops.parallel_for import pfor as pfor__


class transformed_data_model(tfd__.Distribution):

  def __init__(self, X):
    self.X = X
    self.X2 = X + tf__.cast(1, tf__.float64)
     
  
  def log_prob_one_chain(self, params):
    target = 0
    
    # Data
    X = self.X
    
    # Transformed data
    X2 = self.X2
    
    # Parameters
    p = tf__.cast(params[0], tf__.float64)
    
    # Target log probability computation
    target += tf__.reduce_sum(tfd__.Bernoulli(None, p).log_prob(X2))
    return target
     
  def log_prob(self, params):
    return tf__.vectorized_map(self.log_prob_one_chain, params)
    
     
  def parameter_shapes(self, nchains__):
    X = self.X
    return [(nchains__, )]
     
  def parameter_bijectors(self):
    X = self.X
    return [tfb__.Chain([tfb__.Sigmoid(tf__.cast(0, tf__.float64),
                                       tf__.cast(1, tf__.float64))])]
     
  def parameter_names(self):
    return ["p"]
     
model = transformed_data_model