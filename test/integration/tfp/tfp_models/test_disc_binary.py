
import numpy as np__
import tensorflow as tf__
import tensorflow_probability as tfp__
tfd__ = tfp__.distributions
tfb__ = tfp__.bijectors
from tensorflow.python.ops.parallel_for import pfor as pfor__

class test_disc_binary_model(tfd__.Distribution):

  def __init__(self, y_bern):
    self.y_bern = y_bern
     
  
  def log_prob_one_chain(self, params):
    target = 0
    y_bern = self.y_bern
    p_bern = tf__.cast(params[0], tf__.float64)
    p_bern_logit = tf__.cast(params[1], tf__.float64)
    target += tf__.reduce_sum(tfd__.Bernoulli(None, p_bern).log_prob(y_bern))
    target += tf__.reduce_sum(tfd__.Bernoulli(p_bern_logit).log_prob(y_bern))
    return target
     
  def log_prob(self, params):
    return tf__.vectorized_map(self.log_prob_one_chain, params)
    
     
  def parameter_shapes(self, nchains__):
    y_bern = self.y_bern
    return [(nchains__, ), (nchains__, )]
     
  def parameter_bijectors(self):
    y_bern = self.y_bern
    return [tfb__.Chain([tfb__.Shift(tf__.cast(0, tf__.float64)),
                         tfb__.Scale(tf__.cast(1, tf__.float64) - tf__.cast(0, tf__.float64)),
                         tfb__.Sigmoid()]), tfb__.Identity()]
     
  def parameter_names(self):
    return ["p_bern", "p_bern_logit"]
     
model = test_disc_binary_model