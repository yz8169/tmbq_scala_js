package filters

import javax.inject.Inject

import play.api.http.DefaultHttpFilters
import play.filters.gzip.GzipFilter

/**
  * Created by yz on 2017/6/13.
  */

class Filters @Inject()(admin: AdminFilter, gzipFilter: GzipFilter) extends DefaultHttpFilters(admin, gzipFilter)
