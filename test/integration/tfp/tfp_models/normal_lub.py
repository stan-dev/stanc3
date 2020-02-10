
import numpy as np__
import tensorflow as tf__
import tensorflow_probability as tfp__
tfd__ = tfp__.distributions
tfb__ = tfp__.bijectors
from tensorflow.python.ops.parallel_for import pfor as pfor__

class normal_lub_model(tfd__.Distribution):

  def __init__(self, y_lub, y_ub, y_lb):
    self.y_lub = tf__.cast(y_lub, tf__.float64)
    self.y_ub = tf__.cast(y_ub, tf__.float64)
    self.y_lb = tf__.cast(y_lb, tf__.float64)
    
     
  
  def log_prob_one_chain(self, params):
    target = 0
    
    # Data
    y_lub = self.y_lub
    y_ub = self.y_ub
    y_lb = self.y_lb
    
    # Transformed data
    
    
    # Parameters
    theta_lub = tf__.cast(params[0], tf__.float64)
    theta_ub = tf__.cast(params[1], tf__.float64)
    theta_lb = tf__.cast(params[2], tf__.float64)
    
    # Target log probability computation
    target += tf__.reduce_sum(tfd__.Normal(tf__.cast(0, tf__.float64),
                                           tf__.cast(5, tf__.float64)).log_prob(theta_lub))
    target += tf__.reduce_sum(tfd__.Normal(tf__.cast(0, tf__.float64),
                                           tf__.cast(5, tf__.float64)).log_prob(theta_ub))
    target += tf__.reduce_sum(tfd__.Normal(tf__.cast(0, tf__.float64),
                                           tf__.cast(5, tf__.float64)).log_prob(theta_lb))
    target += tf__.reduce_sum(tfd__.Normal(theta_lub,
                                           tf__.cast(1, tf__.float64)).log_prob(y_lub))
    target += tf__.reduce_sum(tfd__.Normal(theta_ub,
                                           tf__.cast(1, tf__.float64)).log_prob(y_ub))
    target += tf__.reduce_sum(tfd__.Normal(theta_lb,
                                           tf__.cast(1, tf__.float64)).log_prob(y_lb))
    return target
     
  def log_prob(self, params):
    return tf__.vectorized_map(self.log_prob_one_chain, params)
    
     
  def parameter_shapes(self, nchains__):
    y_lub = self.y_lub
    y_ub = self.y_ub
    y_lb = self.y_lb
    return [(nchains__, ), (nchains__, ), (nchains__, )]
     
  def parameter_bijectors(self):
    y_lub = self.y_lub
    y_ub = self.y_ub
    y_lb = self.y_lb
    return [tfb__.Chain([tfb__.Shift(-tf__.cast(3, tf__.float64)),
                         tfb__.Scale(tf__.cast(3, tf__.float64) - (-tf__.cast(3, tf__.float64))),
                         tfb__.Sigmoid()]),
            tfb__.Chain([tfb__.Shift(tf__.cast(1, tf__.float64)),
                         tfb__.Scale(tf__.cast(-1., tf__.float64)),
                         tfb__.Exp()]),
            tfb__.Chain([tfb__.Shift(tf__.cast(0, tf__.float64)), tfb__.Exp()])]
     
  def parameter_names(self):
    return ["theta_lub", "theta_ub", "theta_lb"]
     
model = normal_lub_model