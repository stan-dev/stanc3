
import numpy as np__
import tensorflow as tf__
import tensorflow_probability as tfp__
tfd__ = tfp__.distributions
tfb__ = tfp__.bijectors
from tensorflow.python.ops.parallel_for import pfor as pfor__

class test_positive_cont_model(tfd__.Distribution):

  def __init__(self, N, y):
    self.N = N
    self.y = tf__.cast(y, tf__.float64)
     
  
  def log_prob_one_chain(self, params):
    target = 0
    N = self.N
    y = self.y
    loc_lognormal = tf__.cast(params[0], tf__.float64)
    scale_lognormal = tf__.cast(params[1], tf__.float64)
    chi_square_nu = tf__.cast(params[2], tf__.float64)
    exp_rate = tf__.cast(params[3], tf__.float64)
    gamma_alpha = tf__.cast(params[4], tf__.float64)
    gamma_beta = tf__.cast(params[5], tf__.float64)
    inv_gamma_alpha = tf__.cast(params[6], tf__.float64)
    inv_gamma_beta = tf__.cast(params[7], tf__.float64)
    target += tf__.reduce_sum(tfd__.Normal(tf__.cast(0, tf__.float64),
                                           tf__.cast(5, tf__.float64)).log_prob(loc_lognormal))
    target += tf__.reduce_sum(tfd__.Normal(tf__.cast(0, tf__.float64),
                                           tf__.cast(5, tf__.float64)).log_prob(scale_lognormal))
    target += tf__.reduce_sum(tfd__.Gamma(tf__.cast(2, tf__.float64),
                                          tf__.cast(0.1, tf__.float64)).log_prob(chi_square_nu))
    target += tf__.reduce_sum(tfd__.Normal(tf__.cast(0, tf__.float64),
                                           tf__.cast(5, tf__.float64)).log_prob(exp_rate))
    target += tf__.reduce_sum(tfd__.Normal(tf__.cast(0, tf__.float64),
                                           tf__.cast(5, tf__.float64)).log_prob(gamma_alpha))
    target += tf__.reduce_sum(tfd__.Normal(tf__.cast(0, tf__.float64),
                                           tf__.cast(5, tf__.float64)).log_prob(gamma_beta))
    target += tf__.reduce_sum(tfd__.Normal(tf__.cast(0, tf__.float64),
                                           tf__.cast(5, tf__.float64)).log_prob(inv_gamma_alpha))
    target += tf__.reduce_sum(tfd__.Normal(tf__.cast(0, tf__.float64),
                                           tf__.cast(5, tf__.float64)).log_prob(inv_gamma_beta))
    target += tf__.reduce_sum(tfd__.LogNormal(loc_lognormal, scale_lognormal).log_prob(y))
    target += tf__.reduce_sum(tfd__.Chi2(chi_square_nu).log_prob(y))
    target += tf__.reduce_sum(tfd__.Exponential(exp_rate).log_prob(y))
    target += tf__.reduce_sum(tfd__.Gamma(gamma_alpha, gamma_beta).log_prob(y))
    target += tf__.reduce_sum(tfd__.InverseGamma(inv_gamma_alpha,
                                                 inv_gamma_beta).log_prob(y))
    return target
     
  def log_prob(self, params):
    return tf__.vectorized_map(self.log_prob_one_chain, params)
    
     
  def parameter_shapes(self, nchains__):
    N = self.N
    y = self.y
    return [(nchains__, ), (nchains__, ), (nchains__, ), (nchains__, 
            ), (nchains__, ), (nchains__, ), (nchains__, ), (nchains__, 
            )]
     
  def parameter_bijectors(self):
    N = self.N
    y = self.y
    return [tfb__.Identity(),
            tfb__.Chain([tfb__.AffineScalar(tf__.cast(0, tf__.float64)),
                         tfb__.Exp()]),
            tfb__.Chain([tfb__.AffineScalar(tf__.cast(0, tf__.float64)),
                         tfb__.Exp()]),
            tfb__.Chain([tfb__.AffineScalar(tf__.cast(0, tf__.float64)),
                         tfb__.Exp()]),
            tfb__.Chain([tfb__.AffineScalar(tf__.cast(0, tf__.float64)),
                         tfb__.Exp()]),
            tfb__.Chain([tfb__.AffineScalar(tf__.cast(0, tf__.float64)),
                         tfb__.Exp()]),
            tfb__.Chain([tfb__.AffineScalar(tf__.cast(0, tf__.float64)),
                         tfb__.Exp()]),
            tfb__.Chain([tfb__.AffineScalar(tf__.cast(0, tf__.float64)),
                         tfb__.Exp()])]
     
  def parameter_names(self):
    return ["loc_lognormal", "scale_lognormal", "chi_square_nu", "exp_rate",
            "gamma_alpha", "gamma_beta", "inv_gamma_alpha", "inv_gamma_beta"]
     
model = test_positive_cont_model