#!/usr/bin/env python
# -*- coding: utf-8 -*-

'''
A sugestão para isso começar a funcionar
seria afastar os números do eixo e
após isso desenhar os quadrados brancos
e pretos intercalados entre as distâncias
dos ticks
'''
import matplotlib.pyplot as plt
import numpy as np

fig = plt.figure()
ax = fig.add_subplot(111)
ax.tick_params(direction='in', length=6, width=2, colors='g',pad=22)
plt.show()

