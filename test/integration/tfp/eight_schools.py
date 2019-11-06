
import numpy as np__
import tensorflow as tf__
import tensorflow_probability as tfp__
tfd__ = tfp__.distributions
tfb__ = tfp__.bijectors
dtype__ = tf__.float64

class eight_schools_ncp_model(tfd__.Distribution):

  def __init__(self, J, y, sigma):
    self.J = J
    self.y = tf__.cast(y, dtype__)
    self.sigma = tf__.cast(sigma, dtype__)
     
  
  def log_prob_one_chain(self, params):
    target = 0
    J = self.J
    y = self.y
    sigma = self.sigma
    mu = tf__.cast(params[0], dtype__)
    tau = tf__.cast(params[1], dtype__)
    theta_tilde = tf__.cast(params[2], dtype__)
    theta = (mu + (tau * theta_tilde))
    target += tf__.reduce_sum(
          tfd__.Normal(tf__.cast(0, dtype__), tf__.cast(5, dtype__))
            .log_prob(mu))
    target += tf__.reduce_sum(
          tfd__.Normal(tf__.cast(0, dtype__), tf__.cast(5, dtype__))
            .log_prob(tau))
    target += tf__.reduce_sum(
          tfd__.Normal(tf__.cast(0, dtype__), tf__.cast(1, dtype__))
            .log_prob(theta_tilde))
    target += tf__.reduce_sum(tfd__.Normal(theta, sigma).log_prob(y))
    return target
     
  def log_prob(self, params):
    return tf__.vectorized_map(self.log_prob_one_chain, params)
    
     
  def parameter_shapes(self, nchains__):
    J = self.J
    y = self.y
    sigma = self.sigma
    return [(nchains__, ), (nchains__, ), (nchains__, J)]
     
  def parameter_bijectors(self):
    J = self.J
    y = self.y
    sigma = self.sigma
    return [tfb__.Identity(),
            tfb__.Chain([tfb__.AffineScalar(tf__.cast(0, dtype__)),
                         tfb__.Exp()]), tfb__.Identity()]
     
  @staticmethod
  def parameter_names():
    return ["mu", "tau", "theta_tilde"]
     
model = eight_schools_ncp_model