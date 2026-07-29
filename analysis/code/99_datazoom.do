*microdados do Censo via DataZoom
// assim também serve //

*importante
set more off


net from http://www.econ.puc-rio.br/datazoom/portugues

*usar a própria caixa de diálogo
db datazoom_censo

append using CENSO10_AC_dom_comp
append using CENSO10_AL_dom_comp
append using CENSO10_AM_dom_comp
append using CENSO10_AP_dom_comp
append using CENSO10_BA_dom_comp
append using CENSO10_CE_dom_comp
append using CENSO10_DF_dom_comp
append using CENSO10_ES_dom_comp
append using CENSO10_GO_dom_comp
append using CENSO10_MA_dom_comp
append using CENSO10_MG_dom_comp
append using CENSO10_MS_dom_comp
append using CENSO10_MT_dom_comp
append using CENSO10_PA_dom_comp
append using CENSO10_PB_dom_comp
append using CENSO10_PE_dom_comp
append using CENSO10_PI_dom_comp
append using CENSO10_PR_dom_comp
append using CENSO10_RJ_dom_comp
append using CENSO10_RN_dom_comp
append using CENSO10_RO_dom_comp
append using CENSO10_RR_dom_comp
append using CENSO10_RS_dom_comp
append using CENSO10_SC_dom_comp
append using CENSO10_SE_dom_comp
append using CENSO10_SP_dom_comp
append using CENSO10_TO_dom_comp

sort munic

*vamos lá
g id=_n
egen tag = tag(id)
egen distinct = total(tag), by(munic)

by munic: tab ilum_eletr

collapse (sum) ilum_eletr, by(munic codmun nome)

collapse (sum) tot_comodos tot_dorm sanitario tipo_esc_san_B agua_canal dest_lixo ilum_eletr renda_dom banheiros tipo_esc_san abast_agua, by(UF munic codmun nome distinct)





