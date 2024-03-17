Utilizando la estimación central de pobreza semestral del Nowcast de Martín Rozada (https://www.utdt.edu/ver_contenido.php?id_contenido=22217&id_item_menu=36605), utilicé el método de construcción de splines 'MonoH.FC' para intentar predecir la pobreza mensual instantánea. Incluyo en el gráfico el rolling mean alineado a la derecha (es decir el promedio de las predicciones para los meses -5,-4,-3,-2,-1 y 0 respecto de cada mes). Ese promedio debería coincidir con la pobreza semestral del nowcast. El coeficiente de determinación entre el promedio semestral de las predicciones y el nowcast (R2) es de 0.993.

El gráfico se ve así:
![plot](https://github.com/rquiroga7/pobreza_eph_estimacion_mensual/blob/main/prediccion_pobreza_mensual.png)
