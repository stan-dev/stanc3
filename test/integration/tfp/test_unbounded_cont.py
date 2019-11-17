
import numpy as np__
import tensorflow as tf__
import tensorflow_probability as tfp__
tfd__ = tfp__.distributions
tfb__ = tfp__.bijectors
from tensorflow.python.ops.parallel_for import pfor as pfor__

class test_unbounded_cont_model(tfd__.Distribution):

  def __init__(self, N, y):
    self.N = N
    self.y = tf__.cast(y, tf__.float64)
     
  
  def log_prob_one_chain(self, params):
    target = 0
    N = self.N
    y = self.y
    loc_normal = tf__.cast(params[0], tf__.float64)
    scale_normal = tf__.cast(params[1], tf__.float64)
    loc_cauchy = tf__.cast(params[2], tf__.float64)
    scale_cauchy = tf__.cast(params[3], tf__.float64)
    loc_gumbel = tf__.cast(params[4], tf__.float64)
    scale_gumbel = tf__.cast(params[5], tf__.float64)
    loc_student_t = tf__.cast(params[6], tf__.float64)
    scale_student_t = tf__.cast(params[7], tf__.float64)
    nu = tf__.cast(params[8], tf__.float64)
    loc_laplace = tf__.cast(params[9], tf__.float64)
    scale_laplace = tf__.cast(params[10], tf__.float64)
    target += tf__.reduce_sum(tfd__.Normal(tf__.cast(0, tf__.float64),
                                           tf__.cast(5, tf__.float64)).log_prob(loc_normal))
    target += tf__.reduce_sum(tfd__.Normal(tf__.cast(0, tf__.float64),
                                           tf__.cast(5, tf__.float64)).log_prob(scale_normal))
    target += tf__.reduce_sum(tfd__.Normal(tf__.cast(0, tf__.float64),
                                           tf__.cast(5, tf__.float64)).log_prob(loc_cauchy))
    target += tf__.reduce_sum(tfd__.Normal(tf__.cast(0, tf__.float64),
                                           tf__.cast(5, tf__.float64)).log_prob(scale_cauchy))
    target += tf__.reduce_sum(tfd__.Normal(tf__.cast(0, tf__.float64),
                                           tf__.cast(5, tf__.float64)).log_prob(loc_gumbel))
    target += tf__.reduce_sum(tfd__.Normal(tf__.cast(0, tf__.float64),
                                           tf__.cast(5, tf__.float64)).log_prob(scale_gumbel))
    target += tf__.reduce_sum(tfd__.Normal(tf__.cast(0, tf__.float64),
                                           tf__.cast(5, tf__.float64)).log_prob(loc_student_t))
    target += tf__.reduce_sum(tfd__.Normal(tf__.cast(0, tf__.float64),
                                           tf__.cast(5, tf__.float64)).log_prob(scale_student_t))
    target += tf__.reduce_sum(tfd__.Gamma(tf__.cast(2, tf__.float64),
                                          tf__.cast(0.1, tf__.float64)).log_prob(nu))
    target += tf__.reduce_sum(tfd__.Normal(tf__.cast(0, tf__.float64),
                                           tf__.cast(5, tf__.float64)).log_prob(loc_laplace))
    target += tf__.reduce_sum(tfd__.Normal(tf__.cast(0, tf__.float64),
                                           tf__.cast(5, tf__.float64)).log_prob(scale_laplace))
    target += tf__.reduce_sum(tfd__.Normal(loc_normal, scale_normal).log_prob(y))
    target += tf__.reduce_sum(tfd__.Cauchy(loc_cauchy, scale_cauchy).log_prob(y))
    target += tf__.reduce_sum(tfd__.Gumbel(loc_gumbel, scale_gumbel).log_prob(y))
    target += tf__.reduce_sum(tfd__.StudentT(nu, loc_student_t,
                                             scale_student_t).log_prob(y))
    target += tf__.reduce_sum(tfd__.Laplace(loc_laplace, scale_laplace).log_prob(y))
    return target
     
  def log_prob(self, params):
    return tf__.vectorized_map(self.log_prob_one_chain, params)
    
     
  def parameter_shapes(self, nchains__):
    N = self.N
    y = self.y
    return [(nchains__, ), (nchains__, ), (nchains__, ), (nchains__, 
            ), (nchains__, ), (nchains__, ), (nchains__, ), (nchains__, 
            ), (nchains__, ), (nchains__, ), (nchains__, )]
     
  def parameter_bijectors(self):
    N = self.N
    y = self.y
    return [tfb__.Identity(),
            tfb__.Chain([tfb__.AffineScalar(tf__.cast(0, tf__.float64)),
                         tfb__.Exp()]), tfb__.Identity(),
            tfb__.Chain([tfb__.AffineScalar(tf__.cast(0, tf__.float64)),
                         tfb__.Exp()]), tfb__.Identity(),
            tfb__.Chain([tfb__.AffineScalar(tf__.cast(0, tf__.float64)),
                         tfb__.Exp()]), tfb__.Identity(),
            tfb__.Chain([tfb__.AffineScalar(tf__.cast(0, tf__.float64)),
                         tfb__.Exp()]),
            tfb__.Chain([tfb__.AffineScalar(tf__.cast(0, tf__.float64)),
                         tfb__.Exp()]), tfb__.Identity(),
            tfb__.Chain([tfb__.AffineScalar(tf__.cast(0, tf__.float64)),
                         tfb__.Exp()])]
     
  def parameter_names(self):
    return ["loc_normal", "scale_normal", "loc_cauchy", "scale_cauchy",
            "loc_gumbel", "scale_gumbel", "loc_student_t", "scale_student_t",
            "nu", "loc_laplace", "scale_laplace"]
     
model = test_unbounded_cont_model