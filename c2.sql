select
	name,
    population
    from world.country
    order by Population desc
    limit 10;

select 
	name,
    Continent,
    surfacearea
    from world.country
    where continent = 'Europe'
    order by SurfaceArea
    limit 10;

select
	continent,
	sum(population) as sum_pop
    from world.country
    group by continent
    order by sum_pop desc;
    
select
	distinct(region) as dist_reg
    from world.country;

SELECT COUNT(*)
  FROM world.country
  WHERE Population > 100000000;

SELECT *
  FROM world.country
  WHERE Population > 100000000;

select substr(name, 1, 3) as short_name
from world.country;
    
select region,
	continent,
    concat(region, ' ,', continent) as concat_reg
    from world.country;
    
SELECT UPPER(Name) AS Cap_Name,
    Name
    FROM world.country
    LIMIT 10;
    
select
	continent,
    count(*) as no_countries
    from world.country
    group by continent
    order by no_countries desc;

select region,
	count(*) as no_countries
    from world.country
    where population > 100000
    group by region;

SELECT Continent, SUM(Population) AS Sum_of_Population
  FROM world.country
  GROUP BY Continent
  having Sum_of_Population > 100000000;

select name
from world.country
where population > (select Population
from world.country
where name = 'United States');

select name
from world.country;

select name
from world.country
where continent = (select continent
from world.country
where name = 'Azerbaijan');

select *
from world.country
where name in ('Taiwan', 'Japan');

select *
from world.city
where countrycode in ('TWN', 'KOR');

select
	country.*,
    city.*
from world.country country
inner join(
	select *
    from world.city
    where CountryCode in ('TWN', 'KOR')
) city
	on country.Code = city.CountryCode
    where country.Code in ('TWN', 'JPN');

select 
	country.*,
    city.*
from world.country country
left join(
	select *
    from world.city
    where CountryCode in ('TWN', 'KOR')
) city
	on country.Code = city.CountryCode
    where country.Code in ('TWN', 'JPN');

SELECT Name
    FROM world.country
    WHERE Code = 'TWN'
UNION
SELECT Name
    FROM world.city
    WHERE CountryCode = 'TWN';

select
	continent,
	sum(population) as sum_of_population
    from world.country
    where continent in ('Asia', 'Europe', 'South America')
    group by Continent;

select Continent, MAX(Population) as MAX_population
from world.country
group by Continent;

select Name
from world.country
where continent in (
	select continent
    from world.country
    where name in ('Argentina', 'Australia')
);

select
	city.Name as City,
    country.name as Country,
    country.Continent
    from (
		select CountryCode,
        name
        from world.city
        where CountryCode = 'TWN'    
    ) city
    left join (
		select
		code,
        name,
        continent
        from world.country
        where Code = 'TWN'
    ) country
    on city.countryCode = country.Code

select
	city.Name as City,
    country.name as Country,
    country.Continent
    from (
		select
			CountryCode,
            Name
            from world.city
            where CountryCode = 'TWN'
    ) city
    left join (
		select
			Code,
            Name,
            Continent
            from world.country
            where Code = 'TWN'
    ) country
    on city.CountryCode = country.Code
union
select
	city.Name as City,
    country.name as Country,
    country.Continent
    from (
		select
			CountryCode,
            Name
            from world.city
            where CountryCode = 'USA'
    ) city
    left join (
		select
			Code,
            Name,
            Continent
            from world.country
            where Code = 'USA'
    ) country
    on city.CountryCode = country.Code;
    
select
	city.Name as City,
    country.name as Country,
    country.Continent
    from (
		select
			CountryCode,
            Name
            from world.city
            where CountryCode in ('TWN', 'USA')
    ) city
    left join (
		select
			Code,
            Name,
            Continent
            from world.country
            where Code in ('TWN', 'USA')
    ) country
    on city.CountryCode = country.Code;



