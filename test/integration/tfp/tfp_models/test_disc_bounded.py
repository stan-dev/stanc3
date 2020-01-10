
import numpy as np__
import tensorflow as tf__
import tensorflow_probability as tfp__
tfd__ = tfp__.distributions
tfb__ = tfp__.bijectors
from tensorflow.python.ops.parallel_for import pfor as pfor__

class test_disc_bounded_model(tfd__.Distribution):

  def __init__(self, n, y):
    self.n = n
    self.y = y
     
  
  def log_prob_one_chain(self, params):
    target = 0
    n = self.n
    y = self.y
    p_binom = tf__.cast(params[0], tf__.float64)
    p_binom_logit = tf__.cast(params[1], tf__.float64)
    target += tf__.reduce_sum(tfd__.Binomial(n, None, p_binom).log_prob(y))
    target += tf__.reduce_sum(tfd__.Binomial(n, p_binom).log_prob(y))
    return target
     
  def log_prob(self, params):
    return tf__.vectorized_map(self.log_prob_one_chain, params)
    
     
  def parameter_shapes(self, nchains__):
    n = self.n
    y = self.y
    return [(nchains__, ), (nchains__, ), (nchains__, n)]
     
  def parameter_bijectors(self):
    n = self.n
    y = self.y
    return [tfb__.Chain([tfb__.Shift(tf__.cast(0, tf__.float64)),
                         tfb__.Scale(tf__.cast(1, tf__.float64) - tf__.cast(0, tf__.float64)),
                         tfb__.Sigmoid()]), tfb__.Identity(),
            tfb__.Identity()]
     
  def parameter_names(self):
    return ["p_binom", "p_binom_logit", "cat_theta_logit"]
     
model = test_disc_bounded_model
import numpy as np__
import tensorflow as tf__
import tensorflow_probability as tfp__
tfd__ = tfp__.distributions
tfb__ = tfp__.bijectors
from tensorflow.python.ops.parallel_for import pfor as pfor__

class test_disc_bounded_model(tfd__.Distribution):

  def __init__(self, n, y):
    self.n = n
    self.y = y
     
  
  def log_prob_one_chain(self, params):
    target = 0
    n = self.n
    y = self.y
    p_binom = tf__.cast(params[0], tf__.float64)
    p_binom_logit = tf__.cast(params[1], tf__.float64)
    target += tf__.reduce_sum(tfd__.Binomial(n, None, p_binom).log_prob(y))
    target += tf__.reduce_sum(tfd__.Binomial(n, p_binom_logit).log_prob(y))
    return target
     
  def log_prob(self, params):
    return tf__.vectorized_map(self.log_prob_one_chain, params)
    
     
  def parameter_shapes(self, nchains__):
    n = self.n
    y = self.y
    return [(nchains__, ), (nchains__, )]
     
  def parameter_bijectors(self):
    n = self.n
    y = self.y
    return [tfb__.Chain([tfb__.Shift(tf__.cast(0, tf__.float64)),
                         tfb__.Scale(tf__.cast(1, tf__.float64) - tf__.cast(0, tf__.float64)),
                         tfb__.Sigmoid()]), tfb__.Identity()]
     
  def parameter_names(self):
    return ["p_binom", "p_binom_logit"]
     
model = test_disc_bounded_model