# Script to compare modelled swe to observations on the ground

ggplot(crhm_output_newsim, aes(datetime, SWE.1)) + geom_line()

plotly::ggplotly()

ggplot(crhm_output_newsim, aes(datetime, m_s_veg.1)) + geom_line()

plotly::ggplotly()

ggplot(crhm_output_newsim, aes(datetime, delunld_int.1)) + geom_line()

plotly::ggplotly()