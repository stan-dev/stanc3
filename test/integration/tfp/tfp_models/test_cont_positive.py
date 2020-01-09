
import numpy as np__
import tensorflow as tf__
import tensorflow_probability as tfp__
tfd__ = tfp__.distributions
tfb__ = tfp__.bijectors
from tensorflow.python.ops.parallel_for import pfor as pfor__

class test_cont_positive_model(tfd__.Distribution):

  def __init__(self):
    pass
     
  
  def log_prob_one_chain(self, params):
    target = 0
    
    p_ln = tf__.cast(params[0], tf__.float64)
    p_chi2 = tf__.cast(params[1], tf__.float64)
    p_exp = tf__.cast(params[2], tf__.float64)
    p_gamma = tf__.cast(params[3], tf__.float64)
    p_inv_gamma = tf__.cast(params[4], tf__.float64)
    target += tf__.reduce_sum(tfd__.LogNormal(tf__.cast(1, tf__.float64),
                                              tf__.cast(1, tf__.float64)).log_prob(p_ln))
    target += tf__.reduce_sum(tfd__.Chi2(tf__.cast(3, tf__.float64)).log_prob(p_chi2))
    target += tf__.reduce_sum(tfd__.Exponential(tf__.cast(2, tf__.float64)).log_prob(p_exp))
    target += tf__.reduce_sum(tfd__.Gamma(tf__.cast(2, tf__.float64),
                                          tf__.cast(1, tf__.float64)).log_prob(p_gamma))
    target += tf__.reduce_sum(tfd__.InverseGamma(tf__.cast(1, tf__.float64),
                                                 tf__.cast(3, tf__.float64)).log_prob(p_inv_gamma))
    return target
     
  def log_prob(self, params):
    return tf__.vectorized_map(self.log_prob_one_chain, params)
    
     
  def parameter_shapes(self, nchains__):
    
    return [(nchains__, ), (nchains__, ), (nchains__, ), (nchains__, 
            ), (nchains__, )]
     
  def parameter_bijectors(self):
    
    return [tfb__.Chain([tfb__.Shift(tf__.cast(0, tf__.float64)), tfb__.Exp()]),
            tfb__.Chain([tfb__.Shift(tf__.cast(0, tf__.float64)), tfb__.Exp()]),
            tfb__.Chain([tfb__.Shift(tf__.cast(0, tf__.float64)), tfb__.Exp()]),
            tfb__.Chain([tfb__.Shift(tf__.cast(0, tf__.float64)), tfb__.Exp()]),
            tfb__.Chain([tfb__.Shift(tf__.cast(0, tf__.float64)), tfb__.Exp()])]
     
  def parameter_names(self):
    return ["p_ln", "p_chi2", "p_exp", "p_gamma", "p_inv_gamma"]
     
model = test_cont_positive_model