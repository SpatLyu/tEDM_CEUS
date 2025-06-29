carbon = readr::read_csv('./data/yearly_carbon.csv') |> 
  #dplyr::filter(year == 2017) |> 
  dplyr::select(year, countyfips, county = County, tem = tavg, carbon = kgco2) |> 
  dplyr::mutate(carbon = carbon / 1e3) |> 
  dplyr::group_split(by = countyfips)

cl = list(tem = do.call(cbind, lapply(carbon, function(df) df$tem)),
          carbon = do.call(cbind, lapply(carbon, function(df) df$carbon)))
tEDM::simplex(cl,"tem","carbon",E = 2:10, k = 6, tau = 1, threads = 8)
tEDM::simplex(cl,"carbon","tem",E = 2:10, k = 6, tau = 1, threads = 8)

carbon = dplyr::group_split(by = countyfips)

c1 = carbon |> dplyr::filter(countyfips == 1003)

tEDM::simplex(c1,"tem","carbon",E = 2:10, k = 6, tau = 1, threads = 8)
tEDM::simplex(c1,"carbon","tem",E = 2:10, k = 6, tau = 1, threads = 8)

tEDM::ccm(c1,"tem","carbon",E = 2, k = 6, tau = 1, libsizes = seq(6,36,6))

tEDM::ic(c1,"carbon","tem",E = 2:10, k = 6:35, tau = 0, threads = 8)

g = tEDM::cmc(c1,"tem","carbon",E = 2, k = 20, tau = 0, threads = 8)

uscounty = sf::read_sf('./data/tl_2010_us_county10/tl_2010_us_county10.shp') |> 
  dplyr::select(countyfips = GEOID10) |> 
  dplyr::mutate(countyfips = as.double(countyfips))

carbon = uscounty |> 
  dplyr::left_join(carbon,by = "countyfips") |> 
  dplyr::filter(dplyr::if_all(c(tem,carbon),\(.x) !is.na(.x)))

sf::write_sf(carbon,'./data/carbon.gdb', overwrite = TRUE)

zip::zip('./data/carbon.zip',
         files = './data/carbon.gdb',
         mode = "cherry-pick")

carbon = sf::read_sf('./data/carbon.gdb/')

spEDM::simplex()

spEDM::sc.test(carbon,"tem","carbon", k = 10)

spEDM::gccm(carbon,"tem", "carbon", E = 3, libsizes = 3000)