
import numpy as np__
import tensorflow as tf__
import tensorflow_probability as tfp__
tfd__ = tfp__.distributions
tfb__ = tfp__.bijectors
from tensorflow.python.ops.parallel_for import pfor as pfor__

class test_disc_unbounded_model(tfd__.Distribution):

  def __init__(self, y):
    self.y = y
     
  
  def log_prob_one_chain(self, params):
    target = 0
    y = self.y
    lam = tf__.cast(params[0], tf__.float64)
    log_lam = tf__.cast(params[1], tf__.float64)
    nb_alpha = tf__.cast(params[2], tf__.float64)
    nb_beta = tf__.cast(params[3], tf__.float64)
    target += tf__.reduce_sum(tfd__.Poisson(lam).log_prob(y))
    target += tf__.reduce_sum(tfd__.Poisson(None, log_lam).log_prob(y))
    target += tf__.reduce_sum(tfd__.NegativeBinomial(nb_alpha, None,
                                                     tf__.cast(1, tf__.float64) / (tf__.cast(1, tf__.float64) + nb_beta)).log_prob(y))
    return target
     
  def log_prob(self, params):
    return tf__.vectorized_map(self.log_prob_one_chain, params)
    
     
  def parameter_shapes(self, nchains__):
    y = self.y
    return [(nchains__, ), (nchains__, ), (nchains__, ), (nchains__, )]
     
  def parameter_bijectors(self):
    y = self.y
    return [tfb__.Chain([tfb__.Shift(tf__.cast(0, tf__.float64)), tfb__.Exp()]),
            tfb__.Identity(),
            tfb__.Chain([tfb__.Shift(tf__.cast(0, tf__.float64)),
                         tfb__.Scale(tf__.cast(10, tf__.float64) - tf__.cast(0, tf__.float64)),
                         tfb__.Sigmoid()]),
            tfb__.Chain([tfb__.Shift(tf__.cast(0, tf__.float64)),
                         tfb__.Scale(tf__.cast(10, tf__.float64) - tf__.cast(0, tf__.float64)),
                         tfb__.Sigmoid()])]
     
  def parameter_names(self):
    return ["lam", "log_lam", "nb_alpha", "nb_beta"]
     
model = test_disc_unbounded_model