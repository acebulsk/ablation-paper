a_u = 5.204024e-06
b_u = 7.363594e-02
u_mid = 1
cl = 15
k_unld = u_mid * a_u * exp(b_u * u_mid)
q_unl = cl * k_unld # mm/s
q_unl_hr = q_unl * 60*60
#TODO check mid canopy wind speed, seems too high