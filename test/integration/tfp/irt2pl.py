
import numpy as np__
import tensorflow as tf__
import tensorflow_probability as tfp__
tfd__ = tfp__.distributions
tfb__ = tfp__.bijectors
dtype__ = tf__.float64

class irt_2pl_model(tfd__.Distribution):

  def __init__(self, I, J, y):
    self.I = I
    self.J = J
    self.y = y
     
  
  def log_prob_one_chain(self, params):
    target = 0
    I = self.I
    J = self.J
    y = self.y
    sigma_theta = tf__.cast(params[0], dtype__)
    theta = tf__.cast(params[1], dtype__)
    sigma_a = tf__.cast(params[2], dtype__)
    a = tf__.cast(params[3], dtype__)
    mu_b = tf__.cast(params[4], dtype__)
    sigma_b = tf__.cast(params[5], dtype__)
    b = tf__.cast(params[6], dtype__)
    target += tf__.reduce_sum(
      tfd__.Cauchy(tf__.cast(0, dtype__), tf__.cast(2, dtype__)).log_prob(
        sigma_theta))
    target += tf__.reduce_sum(
      tfd__.Normal(tf__.cast(0, dtype__), sigma_theta).log_prob(theta))
    target += tf__.reduce_sum(
      tfd__.Cauchy(tf__.cast(0, dtype__), tf__.cast(2, dtype__)).log_prob(
        sigma_a))
    target += tf__.reduce_sum(
      tfd__.LogNormal(tf__.cast(0, dtype__), sigma_a).log_prob(a))
    target += tf__.reduce_sum(
      tfd__.Normal(tf__.cast(0, dtype__), tf__.cast(5, dtype__)).log_prob(
        mu_b))
    target += tf__.reduce_sum(
      tfd__.Cauchy(tf__.cast(0, dtype__), tf__.cast(2, dtype__)).log_prob(
        sigma_b))
    target += tf__.reduce_sum(tfd__.Normal(mu_b, sigma_b).log_prob(b))
    cond_sym1__ = lambda i, _: tf__.less(i, I + 1)
    def body_sym1__(i, target):
      target += tf__.reduce_sum(
        tfd__.Bernoulli(
              (tf__.gather(a, (i - 1)) * (theta - tf__.gather(b, (i - 1)))))
          .log_prob(tf__.gather(y, (i - 1))))
      return [i + 1, target]
       
    target += tf__.while_loop(cond_sym1__, body_sym1__, [1, 0])[1]
    return target
     
  def log_prob(self, params):
    return tf__.vectorized_map(self.log_prob_one_chain, params)
    
     
  def parameter_shapes(self, nchains__):
    I = self.I
    J = self.J
    y = self.y
    return [(nchains__, ), (nchains__, J), (nchains__, ), (nchains__, 
            I), (nchains__, ), (nchains__, ), (nchains__, I)]
     
  def parameter_bijectors(self):
    I = self.I
    J = self.J
    y = self.y
    return [tfb__.Chain([tfb__.AffineScalar(tf__.cast(0, dtype__)),
                         tfb__.Exp()]), tfb__.Identity(),
            tfb__.Chain([tfb__.AffineScalar(tf__.cast(0, dtype__)),
                         tfb__.Exp()]),
            tfb__.Chain([tfb__.AffineScalar(tf__.cast(0, dtype__)),
                         tfb__.Exp()]), tfb__.Identity(),
            tfb__.Chain([tfb__.AffineScalar(tf__.cast(0, dtype__)),
                         tfb__.Exp()]), tfb__.Identity()]
     
  @staticmethod
  def parameter_names():
    return ["sigma_theta", "theta", "sigma_a", "a", "mu_b", "sigma_b", "b"]
     
model = irt_2pl_model