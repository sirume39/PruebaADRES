--select depa_nombre, count(depa_nombre) as Cantidad from Prestadores group by depa_nombre; 

--select muni_nombre, count(muni_nombre) as Cantidad from Prestadores group by muni_nombre;

--select muni_nombre, count(muni_nombre) as Cantidad, Poblacion from Prestadores as p, Municipios as m where m.Municipio = p.muni_nombre group by muni_nombre; 

--SELECT clpr_nombre, count(clpr_nombre) as Cantidad from Prestadores GROUP by clpr_nombre;

--select Region, count(muni_nombre) as Cantidad from Prestadores as p, Municipios as m where m.Municipio = p.muni_nombre group by Region;

--SELECT caracter, count(caracter) as Cantidad from Prestadores where caracter is not NULL GROUP by caracter;
--SELECT nivel, count(nivel) as Cantidad from Prestadores where nivel is not NULL GROUP by nivel;

--SELECT clpr_nombre,fecha_radicacion, fecha_vencimiento from Prestadores;

--SELECT * from Prestadores where muni_nombre = "guadalajaradebuga";

--select muni_nombre, count(muni_nombre) as Cantidad from Prestadores group by muni_nombre; 

--select Municipio, Poblacion from Municipios;