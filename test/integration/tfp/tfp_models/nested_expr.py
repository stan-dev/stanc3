
import numpy as np__
import tensorflow as tf__
import tensorflow_probability as tfp__
tfd__ = tfp__.distributions
tfb__ = tfp__.bijectors
from tensorflow.python.ops.parallel_for import pfor as pfor__


class nested_expr_model(tfd__.Distribution):

  def __init__(self, x, y):
    self.x = tf__.cast(x, tf__.float64)
    self.y = tf__.cast(y, tf__.float64)
    
     
  
  def log_prob_one_chain(self, params):
    target = 0
    
    # Data
    x = self.x
    y = self.y
    
    # Transformed data
    
    
    # Parameters
    s = tf__.cast(params[0], tf__.float64)
    
    # Target log probability computation
    z = x if (y < x) else (y if (tf__.cast(1, tf__.float64) < y) else tf__.cast(1, tf__.float64))
    v = z * (z + (x ** tf__.cast(2, tf__.float64)))
    u = ((y < tf__.cast(1, tf__.float64)) and (x < tf__.cast(1, tf__.float64))) * (-x)
    target += tf__.reduce_sum(tfd__.Normal(u, v).log_prob(s))
    return target
     
  def log_prob(self, params):
    return tf__.vectorized_map(self.log_prob_one_chain, params)
    
     
  def parameter_shapes(self, nchains__):
    x = self.x
    y = self.y
    return [(nchains__, )]
     
  def parameter_bijectors(self):
    x = self.x
    y = self.y
    return [tfb__.Identity()]
     
  def parameter_names(self):
    return ["s"]
     
model = nested_expr_model