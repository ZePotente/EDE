# Script para graficar la solución de una EDP-Elíptica
# Ultima actualización: 13/05/14

set terminal png
set output 'EDE.png'

set pm3d map
set nokey
set title "Distribución de Temperaturas en una placa rectangular"
splot 'Solucion EDE.txt'
