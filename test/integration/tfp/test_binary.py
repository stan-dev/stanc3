
import numpy as np__
import tensorflow as tf__
import tensorflow_probability as tfp__
tfd__ = tfp__.distributions
tfb__ = tfp__.bijectors
from tensorflow.python.ops.parallel_for import pfor as pfor__

class test_binary_model(tfd__.Distribution):

  def __init__(self, n_binom, y_binom, y_bern):
    self.n_binom = n_binom
    self.y_binom = y_binom
    self.y_bern = y_bern
     
  
  def log_prob_one_chain(self, params):
    target = 0
    n_binom = self.n_binom
    y_binom = self.y_binom
    y_bern = self.y_bern
    p_binom = tf__.cast(params[0], tf__.float64)
    p_bern = tf__.cast(params[1], tf__.float64)
    target += tf__.reduce_sum(tfd__.Binomial(n_binom, None, p_binom).log_prob(y_binom))
    target += tf__.reduce_sum(tfd__.Bernoulli(None, p_bern).log_prob(y_bern))
    return target
     
  def log_prob(self, params):
    return tf__.vectorized_map(self.log_prob_one_chain, params)
    
     
  def parameter_shapes(self, nchains__):
    n_binom = self.n_binom
    y_binom = self.y_binom
    y_bern = self.y_bern
    return [(nchains__, ), (nchains__, )]
     
  def parameter_bijectors(self):
    n_binom = self.n_binom
    y_binom = self.y_binom
    y_bern = self.y_bern
    return [tfb__.Chain([tfb__.Shift(tf__.cast(0, tf__.float64)),
                         tfb__.Scale(tf__.cast(1, tf__.float64) - tf__.cast(0, tf__.float64)),
                         tfb__.Sigmoid()]),
            tfb__.Chain([tfb__.Shift(tf__.cast(0, tf__.float64)),
                         tfb__.Scale(tf__.cast(1, tf__.float64) - tf__.cast(0, tf__.float64)),
                         tfb__.Sigmoid()])]
     
  def parameter_names(self):
    return ["p_binom", "p_bern"]
     
model = test_binary_model