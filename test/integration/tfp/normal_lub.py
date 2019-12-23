
import numpy as np__
import tensorflow as tf__
import tensorflow_probability as tfp__
tfd__ = tfp__.distributions
tfb__ = tfp__.bijectors
dtype__ = tf__.float64

class normal_lub_model(tfd__.Distribution):

  def __init__(self, y_lub, y_ub, y_lb):
    self.y_lub = tf__.convert_to_tensor(y_lub, dtype=dtype__)
    self.y_ub = tf__.convert_to_tensor(y_ub, dtype=dtype__)
    self.y_lb = tf__.convert_to_tensor(y_lb, dtype=dtype__)
     
  
  def log_prob_one_chain(self, params):
    target = 0
    y_lub = self.y_lub
    y_ub = self.y_ub
    y_lb = self.y_lb
    theta_lub = tf__.cast(params[0], dtype__)
    theta_ub = tf__.cast(params[1], dtype__)
    theta_lb = tf__.cast(params[2], dtype__)
    target += tf__.reduce_sum(
      tfd__.Normal(tf__.cast(0, dtype__), tf__.cast(5, dtype__)).log_prob(
        theta_lub))
    target += tf__.reduce_sum(
      tfd__.Normal(tf__.cast(0, dtype__), tf__.cast(5, dtype__)).log_prob(
        theta_ub))
    target += tf__.reduce_sum(
      tfd__.Normal(tf__.cast(0, dtype__), tf__.cast(5, dtype__)).log_prob(
        theta_lb))
    target += tf__.reduce_sum(
      tfd__.Normal(theta_lub, tf__.cast(1, dtype__)).log_prob(y_lub))
    target += tf__.reduce_sum(
      tfd__.Normal(theta_ub, tf__.cast(1, dtype__)).log_prob(y_ub))
    target += tf__.reduce_sum(
      tfd__.Normal(theta_lb, tf__.cast(1, dtype__)).log_prob(y_lb))
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
    return [tfb__.Chain([tfb__.Shift((-tf__.cast(3, dtype__))),
                         tfb__.Scale(
                               (tf__.cast(3, dtype__) -
                                 (-tf__.cast(3, dtype__)))), tfb__.Sigmoid()]),
            tfb__.Chain([tfb__.Shift(tf__.cast(1, dtype__)),
                         tfb__.Scale(tf__.cast(-1., dtype__)), tfb__.Exp()]),
            tfb__.Chain([tfb__.Shift(tf__.cast(0, dtype__)), tfb__.Exp()])]
     
  @staticmethod
  def parameter_names():
    return ["theta_lub", "theta_ub", "theta_lb"]
     
model = normal_lub_model