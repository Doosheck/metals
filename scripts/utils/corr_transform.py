# plot how a transformation sqrt(2(1-x)) affects the transformation
# plot x from -1 to 1
import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(-1, 1, 1000)
y = np.sqrt(2 * (1 - x))

plt.figure(figsize=(10, 6))
plt.plot(x, y, label=r'$y = \sqrt{2(1-x)}$', color='blue')
plt.title('Transformation: $y = \sqrt{2(1-x)}$')
plt.xlabel('x')
plt.ylabel('y')
plt.axhline(0, color='black', lw=0.5, ls='--')
plt.axvline(0, color='black', lw=0.5, ls='--')
plt.grid()
plt.legend()
plt.show()