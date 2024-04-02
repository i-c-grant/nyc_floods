from datetime import datetime, timedelta 
from siphon.radarserver import RadarServer

rs = RadarServer('http://tds-nexrad.scigw.unidata.ucar.edu/thredds/radarServer/nexrad/level2/S3/')

# check attributes of rs
vars(rs)

query = rs.query()
month_ago = datetime.utcnow() - timedelta(days=30)
query.stations('KOKX').time_range(month_ago, datetime.utcnow())
rs.validate_query(query)
catalog = rs.get_catalog(query)
vars(catalog)

