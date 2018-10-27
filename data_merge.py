import pandas as pd
import numpy as np

loc = 'home/gregory/Projects/DataOpen/dat/'

calendar_dat = pd.read_csv(loc + 'calendar.csv')
listings_dat = pd.read_csv(loc + 'listings.csv')
econ_state_dat = pd.read_csv(loc + 'econ_state.csv')
demographics_dat = pd.read_csv(loc + 'demographics.csv')
venues_dat = pd.read_csv(loc + 'venues.csv')
real_estate_dat = pd.read_csv(loc + 'real_estate.csv')

listings_dat


listings_dat = listings_dat[['id', 'host_id', 'accommodates', 'availability_30', 'bedrooms',
                             'latitude', 'longitude', 'property_type', 'city', 'state', 'zipcode', 'room_type',
                             'instant_bookable']]

airbnb_dat = pd.merge(listings_dat, calendar_dat, how='left', left_on='id', right_on='listing_id')

id_group = airbnb_dat.groupby('id')

airbnb_dat['count'] =