/********************************************************************
**** 一、整理数据
********************************************************************/
clear all
set more off
set linesize 120
version 18.0

cd "Insert appropriate file path"
*============================================================*
* 0. 载入数据
*============================================================*
use "reg3.0.dta", clear

*============================================================*
* 1. 添加数据标签
*============================================================*
label variable iso_o "来源国的iso三位编码"
label variable iso_d "目的国的iso三位编码"
label variable year "年份"
label variable signed "两国是否签订贸易协定（生效），0为未签订，1为签订"
label variable breadth "两国签订贸易协定的广度"
label variable depth "两国签订贸易协定的深度"
label variable ipd "两国间的理想点距离"
label variable idealpoint_o "来源国的理想点"
label variable idealpoint_d "目的国的理想点"
label variable group_o "来源国所属的集团类型，1为USA集团，2为CHN集团，3为不结盟集团"
label variable group_d "目的国所属的集团类型，1为USA集团，2为CHN集团，3为不结盟集团"
label variable type_o "来源国所属的经济体类型，AE为发达经济体，EMDE为新兴市场经济体"
label variable type_d "目的国所属的经济体类型，AE为发达经济体，EMDE为新兴市场经济体"
label variable distcap "两国首都之间的距离，单位（公里）"
label variable contig "两国是否相邻，0为不相邻，1为相邻"
label variable comlang_off "两国是否有共同的官方语言或主要语言，0为不相同，1为相同"
label variable comrelig "两国宗教亲近度，0-1，递增"
label variable col_dep_ever "两国殖民历史关系，0为无殖民关系，1为有殖民关系"
label variable dist "两国每个国家人口最多的城市之间的距离，单位（公里）"
label variable nr_gain_b_od "来源国未实现的贸易收益（基于广度）"
label variable nr_gain_b_do "目的国未实现的贸易收益（基于广度）"
label variable nr_gain_d_od "来源国未实现的贸易收益（基于深度）"
label variable nr_gain_d_do "目的国未实现的贸易收益（基于深度）"
label variable mood "两国间的政治关系，[-1,1]，越大关系越好"
label variable rta_name "两国签署的区域贸易协定名称"
label variable type "两国签署的区域贸易协定类型"


save "reg3.0.dta", replace




/********************************************************************
**** 二、基础回归
********************************************************************/

*============================================================*
* 1. 描述性统计
*============================================================*

use "reg3.0.dta", clear

* 选择目标变量
local desc_vars signed breadth depth ipd nr_gain_b_od nr_gain_b_do nr_gain_d_od nr_gain_d_do distcap comlang_off contig col_dep_ever 

* 移除 listwise 选项，这样每个变量都会根据其自身的非缺失值进行统计
estpost summarize `desc_vars'

* 导出到 Word
esttab . using "Summary_Statistics_Fixed.rtf", replace ///
    cells("count(fmt(0)) mean(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3))") ///
    label noobs nonumber ///
    title("Table: Descriptive Statistics (Variable-specific N)") ///
    collabels("N" "Mean" "SD" "Min" "Max")


*============================================================*
* 2. 基础回归（ols+高维固定效应）
*============================================================*

/********************************************************************
  Table 2
  - 含 dyadic controls
  - 含 固定效应 ot、dt
  - 含 聚类层级 pair
********************************************************************/

* (1) 签不签（nr_gain_b）
preserve
keep if signed==0 & !missing(form_signed)

reghdfe form_signed ///
    ipd nr_gain_b_od nr_gain_b_do ///
	ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T2_form_b

* (2) 签不签（nr_gain_d）
reghdfe form_signed ///
    ipd nr_gain_d_od nr_gain_d_do ///
	ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T2_form_d
restore

* (3) 签多广（幅度）
reghdfe d_breadth ///
    ipd nr_gain_b_od nr_gain_b_do ///
	ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T2_db

* (4) 签多广的可能性
reghdfe deepen_breadth ///
    ipd nr_gain_b_od nr_gain_b_do ///
	ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T2_db_lpm

* (5) 签多深（幅度）
reghdfe d_depth ///
    ipd nr_gain_d_od nr_gain_d_do ///
	ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T2_dd

* (6) 签多深的可能性

reghdfe deepen_depth ///
    ipd nr_gain_d_od nr_gain_d_do ///
	ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T2_dd_lpm

* 导出 Table 2
esttab T2_form_b T2_db_lpm T2_db T2_form_d T2_dd_lpm T2_dd ///
    using "Table2.rtf", replace ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.4f) se(%9.4f) ///
	r2(%9.4f) ///  // 增加 R2 输出，保留四位小数
	order( ///
        ipd ///
        nr_gain_b_od ///
        nr_gain_b_do ///
		nr_gain_d_od ///
        nr_gain_d_do ///
        ln_DIST ///
        comlang_off ///
        contig ///
        col_dep_ever ///
        _cons ///
    ) ///
    title("Table 2. ") ///
    addnotes("Dyadic controls: ln(distcap), comlang_off contig col_dep_ever.", ///
             "Cols(1)(4): Formation sample (signed==0 at t).", ///
             "Col(1) uses nr_gain_b; Col(4) uses nr_gain_d.", ///
             "FE: exporter-year and importer-year.", ///
             "SE: clustered at exporter-importer pair.")
			 
/********************************************************************
  Table 1 
  - 不含 dyadic controls
  - 含 固定效应 pair、t
  - 含 聚类层级 pair
			 
* (1) 签不签（nr_gain_b）
preserve
keep if signed==0 & !missing(form_signed)

reghdfe form_signed ///
    ipd nr_gain_b_od nr_gain_b_do, ///
    absorb(pairid year) ///
    vce(cluster pairid)
est store T1_form_b

* (2) 签不签（nr_gain_d）
reghdfe form_signed ///
    ipd nr_gain_d_od nr_gain_d_do, ///
    absorb(pairid year) ///
    vce(cluster pairid)
est store T1_form_d
restore

* (3) 签多广（幅度）
reghdfe d_breadth ///
    ipd nr_gain_b_od nr_gain_b_do, ///
    absorb(pairid year) ///
    vce(cluster pairid)
est store T1_db

* (4) 签多广的可能性
reghdfe deepen_breadth ///
    ipd nr_gain_b_od nr_gain_b_do, ///
    absorb(pairid year) ///
    vce(cluster pairid)
est store T1_db_lpm

* (5) 签多深（幅度）
reghdfe d_depth ///
    ipd nr_gain_d_od nr_gain_d_do, ///
    absorb(pairid year) ///
    vce(cluster pairid)
est store T1_dd

* (6) 签多深的可能性

reghdfe deepen_depth ///
    ipd nr_gain_d_od nr_gain_d_do, ///
    absorb(pairid year) ///
    vce(cluster pairid)
est store T1_dd_lpm

* 导出 Table 2
esttab T1_form_b T1_db_lpm T1_db T1_form_d T1_dd_lpm T1_dd ///
    using "Table1.rtf", replace ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.4f) se(%9.4f) ///
	r2(%9.4f) ///  // 增加 R2 输出，保留四位小数
	order( ///
        ipd ///
        nr_gain_b_od ///
        nr_gain_b_do ///
		nr_gain_d_od ///
        nr_gain_d_do ///
        _cons ///
    ) ///
    title("Table 1. ") ///
    addnotes("Cols(1)(4): Formation sample (signed==0 at t).", ///
             "Col(1) uses nr_gain_b; Col(4) uses nr_gain_d.", ///
             "FE: pair and year.", ///
             "SE: clustered at exporter-importer pair.")
********************************************************************/	
	
	
	
/********************************************************************
**** 三、稳健性检验
  方法一：更换估计模型（LMP→Probit/两部法）—— X 需要降维且不是很理想
  方法二：LPM 结构变化（换 FE —— ？
                        换聚类 —— √
						换因变量定义 —— ？
						换样本 —— ？）
  方法三：排除欧盟对样本 —— √
  方法四：更换因变量 —— X Gdelt 不显著但方向没问题
  方法五：滞后解释变量 —— √ 滞后三期
  方法六：剔除极端值 —— √ 对ipd进行10–90 百分位缩尾
********************************************************************/			 

*============================================================*
* 方法二：LPM 结构变化——更换聚类层级：从pairid → oyr dyr
*============================================================*			 

* 签不签（nr_gain_b）
preserve
keep if signed==0 & !missing(form_signed)

reghdfe form_signed ///
    ipd nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster oyr dyr)
est store T5_form_b
	
* 签不签（nr_gain_d）
reghdfe form_signed ///
    ipd nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster oyr dyr)
est store T5_form_d
restore

* 签多广（幅度）
reghdfe d_breadth ///
    ipd nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster oyr dyr)
est store T5_db
	
* 签多广的可能性
reghdfe deepen_breadth ///
    ipd nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster oyr dyr)
est store T5_db_lpm

* 签多深（幅度）
reghdfe d_depth ///
    ipd nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster oyr dyr)
est store T5_dd

* 签多深的可能性
reghdfe deepen_depth ///
    ipd nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster oyr dyr)
est store T5_dd_lpm

* 导出 Table 5
esttab T5_form_b T5_db_lpm T5_db T5_form_d T5_dd_lpm T5_dd  ///
    using "Table5_Robustnesschecks.rtf", replace ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.4f) se(%9.4f) ///
	r2(%9.4f) ///  // 增加 R2 输出，保留四位小数
    order( ///
        ipd ///
        nr_gain_b_od ///
        nr_gain_b_do ///
		nr_gain_d_od ///
        nr_gain_d_do ///
        ln_DIST ///
        comlang_off ///
        contig ///
        col_dep_ever ///
        _cons ///
    ) ///
    title("Table 5. Robustness checks") ///
    addnotes("Dyadic controls: ln(distcap), comlang_off contig col_dep_ever.", ///
             "Cols(1)(4): Formation sample (signed==0 at t).", ///
             "Col(1) uses nr_gain_b; Col(4) uses nr_gain_d.", ///
             "FE: exporter-year and importer-year.", ///
             "SE: two-way clustered at exporter-year and importer-year.")
	
	
*============================================================*
* 方法四：更换因变量
*============================================================*				 

preserve
keep if year<=2020
keep if signed==0 & !missing(form_signed)

* 签不签（nr_gain_b）
reghdfe form_signed ///
    mood nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T7_form_b

* 签不签（nr_gain_d）
reghdfe form_signed ///
    mood nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T7_form_d

restore

preserve
keep if year<=2020

* 签多广（幅度）
reghdfe d_breadth ///
    mood nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T7_db

* 签多广的可能性 
reghdfe deepen_breadth ///
    mood nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T7_db_lpm

* 签多深（幅度）
reghdfe d_depth ///
    mood nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T7_dd

* 签多深的可能性
reghdfe deepen_depth ///
    mood nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T7_dd_lpm

* 导出 Table 7
esttab T7_form_b T7_db_lpm T7_db T7_form_d T7_dd_lpm T7_dd ///
    using "Table7_RobustnesschecksGDELT.rtf", replace ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.4f) se(%9.4f) ///
	r2(%9.4f) ///  // 增加 R2 输出，保留四位小数
    order( ///
        mood ///
        nr_gain_b_od ///
        nr_gain_b_do ///
		nr_gain_d_od ///
        nr_gain_d_do ///
        ln_DIST ///
        comlang_off ///
        contig ///
        col_dep_ever ///
        _cons ///
    ) ///
	title("Table 7. Robustness checks GDELT") ///
    addnotes("Dyadic controls: ln(distcap), comlang_off contig col_dep_ever.", ///
             "Cols(1)(4): Formation sample (signed==0 at t).", ///
             "Col(1) uses nr_gain_b; Col(4) uses nr_gain_d.", ///
             "FE: exporter-year and importer-year.", ///
             "SE: clustered at exporter-importer pair.")

restore


*============================================================*
* 方法五：滞后解释变量
*============================================================*	

* 生成三年滞后解释变量
gen ipd_L3        = L3.ipd
gen nr_gain_b_od_L3  = L3.nr_gain_b_od
gen nr_gain_b_do_L3  = L3.nr_gain_b_do
gen nr_gain_d_od_L3  = L3.nr_gain_d_od
gen nr_gain_d_do_L3  = L3.nr_gain_d_do

* 签不签（nr_gain_b）
preserve
keep if signed==0 & !missing(form_signed)

reghdfe form_signed ///
    ipd_L3 nr_gain_b_od_L3 nr_gain_b_do_L3 ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T8_form_b

* 签不签（nr_gain_d）
reghdfe form_signed ///
    ipd_L3 nr_gain_d_od_L3 nr_gain_d_do_L3 ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T8_form_d

restore

* 签多广（幅度）
reghdfe d_breadth ///
    ipd_L3 nr_gain_b_od_L3 nr_gain_b_do_L3 ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T8_db

* 签多广的可能性
reghdfe deepen_breadth ///
    ipd_L3 nr_gain_b_od_L3 nr_gain_b_do_L3 ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T8_db_lpm

* 签多深（幅度）
reghdfe d_depth ///
    ipd_L3 nr_gain_d_od_L3 nr_gain_d_do_L3 ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T8_dd

* 签多深的可能性
reghdfe deepen_depth ///
    ipd_L3 nr_gain_d_od_L3 nr_gain_d_do_L3 ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T8_dd_lpm

* 导出 Table 8
esttab T8_form_b T8_db_lpm T8_db T8_form_d T8_dd_lpm T8_dd   ///
    using "Table8_Robustnesscheckslaggedvalues.rtf", replace ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.4f) se(%9.4f) ///
	r2(%9.4f) ///  // 增加 R2 输出，保留四位小数
	order( ///
        ipd_L3 ///
        nr_gain_b_od_L3 ///
        nr_gain_b_do_L3 ///
		nr_gain_d_od_L3 ///
        nr_gain_d_do_L3 ///
        ln_DIST ///
        comlang_off ///
        contig ///
        col_dep_ever ///
        _cons ///
    ) ///
    title("Table 8. + Robustness checks lagged values") ///
    addnotes("Dyadic controls: ln(distcap), comlang_off contig col_dep_ever.", ///
             "Cols(1)(4): Formation sample (signed==0 at t).", ///
             "Col(1) uses nr_gain_b; Col(4) uses nr_gain_d.", ///
             "FE: exporter-year and importer-year.", ///
             "SE: clustered at exporter-importer pair.")

*============================================================*
* 方法六：剔除极端值
*============================================================*				 

* 查看分位点
summarize ipd, detail

* 生成缩尾版本（10–90 百分位）
gen ipd_w = ipd
* 计算分位点
_pctile ipd, p(10 90)
local p10 = r(r1)
local p90 = r(r2)
* 缩尾（winsorization）
replace ipd_w = `p10' if ipd_w < `p10'
replace ipd_w = `p90' if ipd_w > `p90'


preserve
keep if signed==0 & !missing(form_signed)

* 签不签（nr_gain_b）
reghdfe form_signed ///
    ipd_w nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T9_form_b

* 签不签（nr_gain_d）
reghdfe form_signed ///
    ipd_w nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T9_form_d
restore

* 签多广（幅度）
reghdfe d_breadth ///
    ipd_w nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T9_db

* 签多广的可能性
reghdfe deepen_breadth ///
    ipd_w nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T9_db_lpm

* 签多深（幅度）
reghdfe d_depth ///
    ipd_w nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T9_dd

* 签多深的可能性
reghdfe deepen_depth ///
    ipd_w nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T9_dd_lpm

* 导出 Table 9
esttab T9_form_b T9_db_lpm T9_db  T9_form_d T9_dd_lpm T9_dd   ///
    using "Table9_Robustnesscheckswinsorize.rtf", replace ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.4f) se(%9.4f) ///
	r2(%9.4f) ///  // 增加 R2 输出，保留四位小数
	order( ///
        ipd_w ///
        nr_gain_b_od ///
        nr_gain_b_do ///
		nr_gain_d_od ///
        nr_gain_d_do ///
        ln_DIST ///
        comlang_off ///
        contig ///
        col_dep_ever ///
        _cons ///
    ) ///
    title("Table 9. Robustness checks winsorize") ///
    addnotes("Dyadic controls: ln(distcap), comlang_off contig col_dep_ever.", ///
             "Cols(1)(4): Formation sample (signed==0 at t).", ///
             "Col(1) uses nr_gain_b; Col(4) uses nr_gain_d.", ///
             "FE: exporter-year and importer-year.", ///
             "SE: clustered at exporter-importer pair.")
			 
			 
			 
			 
/********************************************************************
**** 四、异质性检验

  方法一：国家样本的异质性分析——经济体发展阶段（AE/EMDE）
  方法二：贸易协定类型的异质性分析——弱约束/强约束（FTA+FTA&EIA/CU+CU&EIA）
  方法三：双边政治关系距离的异质性分析

********************************************************************/

/********************************************************************
方法一：国家样本角度——经济体发展阶段（AE/EMDE）
********************************************************************/

*============================================================*
* 1. AE–AE
*============================================================*
preserve
keep if type_o=="AE" & type_d=="AE"
keep if signed==0 & !missing(form_signed)

* 签不签（nr_gain_b）
reghdfe form_signed ///
    ipd nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T101_form_b

* 签不签（nr_gain_d）
reghdfe form_signed ///
    ipd nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T101_form_d
restore

preserve
keep if type_o=="AE" & type_d=="AE"

* 签多广（幅度）
reghdfe d_breadth ///
    ipd nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T101_db

* 签多广的可能性 
reghdfe deepen_breadth ///
    ipd nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T101_db_lpm

* 签多深（幅度）
reghdfe d_depth ///
    ipd nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T101_dd

* 签多深的可能性 
reghdfe deepen_depth ///
    ipd nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T101_dd_lpm

esttab T101_form_b T101_db_lpm T101_db  T101_form_d T101_dd_lpm T101_dd   ///
    using "Table101 AE–AE.rtf", replace ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.4f) se(%9.4f) ///
	r2(%9.4f) ///  // 增加 R2 输出，保留四位小数
	order( ///
        ipd ///
        nr_gain_b_od ///
        nr_gain_b_do ///
		nr_gain_d_od ///
        nr_gain_d_do ///
        ln_DIST ///
        comlang_off ///
        contig ///
        col_dep_ever ///
        _cons ///
    ) ///
    title("Table 101. AE–AE ") ///
    addnotes("Dyadic controls: ln(distcap), comlang_off contig col_dep_ever.", ///
             "Cols(1)(4): Formation sample (signed==0 at t).", ///
             "Col(1) uses nr_gain_b; Col(4) uses nr_gain_d.", ///
             "FE: exporter-year and importer-year.", ///
             "SE: clustered at exporter-importer pair.")

restore

*============================================================*
* 2. EMDE–EMDE
*============================================================*
preserve
keep if type_o=="EMDE" & type_d=="EMDE"
keep if signed==0 & !missing(form_signed)

* 签不签（nr_gain_b）
reghdfe form_signed ///
    ipd nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T102_form_b

* 签不签（nr_gain_d）
reghdfe form_signed ///
    ipd nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T102_form_d
restore

preserve
keep if type_o=="EMDE" & type_d=="EMDE"

* 签多广（幅度）
reghdfe d_breadth ///
    ipd nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T102_db

* 签多广的可能性
reghdfe deepen_breadth ///
    ipd nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T102_db_lpm

* 签多深（幅度）
reghdfe d_depth ///
    ipd nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T102_dd

* 签多深的可能性
reghdfe deepen_depth ///
    ipd nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T102_dd_lpm

* 导出 Table 102
esttab T102_form_b T102_db_lpm T102_db T102_form_d T102_dd_lpm T102_dd   ///
    using "Table102 EMDE-EMDE.rtf", replace ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.4f) se(%9.4f) ///
	r2(%9.4f) ///  // 增加 R2 输出，保留四位小数
	order( ///
        ipd ///
        nr_gain_b_od ///
        nr_gain_b_do ///
		nr_gain_d_od ///
        nr_gain_d_do ///
        ln_DIST ///
        comlang_off ///
        contig ///
        col_dep_ever ///
        _cons ///
    ) ///
    title("Table 102.EMDE-EMDE") ///
    addnotes("Dyadic controls: ln(distcap), comlang_off contig col_dep_ever.", ///
             "Cols(1)(4): Formation sample (signed==0 at t).", ///
             "Col(1) uses nr_gain_b; Col(4) uses nr_gain_d.", ///
             "FE: exporter-year and importer-year.", ///
             "SE: clustered at exporter-importer pair.")

restore

*============================================================*
* 3. AE–EMDE
*============================================================*
preserve
keep if type_o!=type_d
keep if signed==0 & !missing(form_signed)

* 签不签（nr_gain_b）
reghdfe form_signed ///
    ipd nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T103_form_b

* 签不签（nr_gain_d）
reghdfe form_signed ///
    ipd nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T103_form_d
restore

preserve
keep if type_o!=type_d

* 签多广（幅度）
reghdfe d_breadth ///
    ipd nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T103_db

* 签多广的可能性 
reghdfe deepen_breadth ///
    ipd nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T103_db_lpm

* 签多深（幅度）
reghdfe d_depth ///
    ipd nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T103_dd

* 签多深的可能性
reghdfe deepen_depth ///
    ipd nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T103_dd_lpm

* 导出 Table 103
esttab T103_form_b T103_db_lpm T103_db T103_form_d T103_dd_lpm T103_dd ///
    using "Table103 AE-EMDE.rtf", replace ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.4f) se(%9.4f) ///
	r2(%9.4f) ///  // 增加 R2 输出，保留四位小数
	order( ///
        ipd ///
        nr_gain_b_od ///
        nr_gain_b_do ///
		nr_gain_d_od ///
        nr_gain_d_do ///
        ln_DIST ///
        comlang_off ///
        contig ///
        col_dep_ever ///
        _cons ///
    ) ///
    title("Table 103.AE-EMDE") ///
    addnotes("Dyadic controls: ln(distcap), comlang_off contig col_dep_ever.", ///
             "Cols(1)(4): Formation sample (signed==0 at t).", ///
             "Col(1) uses nr_gain_b; Col(4) uses nr_gain_d.", ///
             "FE: exporter-year and importer-year.", ///
             "SE: clustered at exporter-importer pair.")
			 
restore

/********************************************************************
方法二：贸易协定类型划分——弱约束/强约束（FTA+FTA&EIA/CU+CU&EIA）
********************************************************************/
* 构造组别：FTA组与CU组
gen byte rta_fta = inlist(type, "FTA", "FTA & EIA")
label var rta_fta "FTA or FTA+EIA"

gen byte rta_cu = inlist(type, "CU", "CU & EIA")
label var rta_cu "CU or CU+EIA"

*============================================================*
* 1. FTA+FTA&EIA
*============================================================*
preserve
keep if rta_fta == 1
keep if signed==0 & !missing(form_signed)

* 签不签（nr_gain_b）
reghdfe form_signed ///
    ipd nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T111_form_b

* 签不签（nr_gain_d）
reghdfe form_signed ///
    ipd nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T111_form_d
restore

preserve
keep if rta_fta == 1

* 签多广（幅度）
reghdfe d_breadth ///
    ipd nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T111_db

* 签多广的可能性
reghdfe deepen_breadth ///
    ipd nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T111_db_lpm

* 签多深（幅度）
reghdfe d_depth ///
    ipd nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T111_dd

* 签多深的可能性
reghdfe deepen_depth ///
    ipd nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T111_dd_lpm

esttab T111_form_b T111_db_lpm T111_db T111_form_d T111_dd_lpm T111_dd ///
    using "Table111 FTA-FTA&EIA.rtf", replace ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.4f) se(%9.4f) ///
	r2(%9.4f) ///  // 增加 R2 输出，保留四位小数
	order( ///
        ipd ///
        nr_gain_b_od ///
        nr_gain_b_do ///
		nr_gain_d_od ///
        nr_gain_d_do ///
        ln_DIST ///
        comlang_off ///
        contig ///
        col_dep_ever ///
        _cons ///
    ) ///
    title("Table 111. FTA-FTA&EIA ") ///
    addnotes("Dyadic controls: ln(distcap), comlang_off contig col_dep_ever.", ///
             "Cols(1)(4): Formation sample (signed==0 at t).", ///
             "Col(1) uses nr_gain_b; Col(4) uses nr_gain_d.", ///
             "FE: exporter-year and importer-year.", ///
             "SE: clustered at exporter-importer pair.")

restore

*============================================================*
* 2. CU+CU&EIA
*============================================================*
preserve
keep if rta_cu == 1
keep if signed==0 & !missing(form_signed)

* 签不签（nr_gain_b）
reghdfe form_signed ///
    ipd nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T112_form_b

* 签不签（nr_gain_d）
reghdfe form_signed ///
    ipd nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T112_form_d
restore

preserve
keep if rta_cu == 1

* 签多广（幅度）
reghdfe d_breadth ///
    ipd nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T112_db

* 签多广的可能性 
reghdfe deepen_breadth ///
    ipd nr_gain_b_od nr_gain_b_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T112_db_lpm

* 签多深（幅度）
reghdfe d_depth ///
    ipd nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T112_dd

* 签多深的可能性
reghdfe deepen_depth ///
    ipd nr_gain_d_od nr_gain_d_do ///
    ln_DIST comlang_off contig col_dep_ever, ///
    absorb(oyr dyr) ///
    vce(cluster pairid)
est store T112_dd_lpm

esttab T112_form_b T112_db_lpm T112_db  T112_form_d T112_dd_lpm T112_dd  ///
    using "Table112 CU+CU&EIA.rtf", replace ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.4f) se(%9.4f) ///
	r2(%9.4f) ///  // 增加 R2 输出，保留四位小数
	order( ///
        ipd ///
        nr_gain_b_od ///
        nr_gain_b_do ///
		nr_gain_d_od ///
        nr_gain_d_do ///
        ln_DIST ///
        comlang_off ///
        contig ///
        col_dep_ever ///
        _cons ///
    ) ///
    title("Table 112. CU+CU&EIA ") ///
    addnotes("Dyadic controls: ln(distcap), comlang_off contig col_dep_ever.", ///
             "Cols(1)(4): Formation sample (signed==0 at t).", ///
             "Col(1) uses nr_gain_b; Col(4) uses nr_gain_d.", ///
             "FE: exporter-year and importer-year.", ///
             "SE: clustered at exporter-importer pair.")
restore



/********************************************************************
方法三：双边政治关系距离的异质性分析
********************************************************************/

*============================================================*
* 1.划分 3 组并生成交互项 
*============================================================*
 
xtile ipd_tile = ipd, n(3)
forvalues i = 1/3 {
    gen group`i' = (ipd_tile == `i')
    gen ipd_g`i' = ipd * group`i'
    label var ipd_g`i' "IPD (Group `i')"
}

* 定义控制变量宏
local dyadic_controls "ln_DIST comlang_off contig col_dep_ever"


*============================================================*
* 2.执行异质性回归
*============================================================*

* 签署概率 (广度收益版)
reghdfe form_signed `dyadic_controls' nr_gain_b_od nr_gain_b_do ///
    ipd_g1 ipd_g2 ipd_g3 ///
    if signed == 0, absorb(oyr dyr) vce(cluster pairid)
est store H_form_b

* 签署概率 (深度收益版)
reghdfe form_signed `dyadic_controls' nr_gain_d_od nr_gain_d_do ///
    ipd_g1 ipd_g2 ipd_g3 ///
    if signed == 0, absorb(oyr dyr) vce(cluster pairid)
est store H_form_d

* 广度增加幅度
reghdfe d_breadth `dyadic_controls' nr_gain_b_od nr_gain_b_do ///
    ipd_g1 ipd_g2 ipd_g3, ///
    absorb(oyr dyr) vce(cluster pairid)
est store H_db

* 广度深化概率
reghdfe deepen_breadth `dyadic_controls' nr_gain_b_od nr_gain_b_do ///
    ipd_g1 ipd_g2 ipd_g3, ///
    absorb(oyr dyr) vce(cluster pairid)
est store H_db_lpm

* 深度增加幅度
reghdfe d_depth `dyadic_controls' nr_gain_d_od nr_gain_d_do  ///
    ipd_g1 ipd_g2 ipd_g3, ///
    absorb(oyr dyr) vce(cluster pairid)
est store H_dd

* 深度深化概率
reghdfe deepen_depth `dyadic_controls' nr_gain_d_od nr_gain_d_do  ///
    ipd_g1 ipd_g2 ipd_g3, ///
    absorb(oyr dyr) vce(cluster pairid)
est store H_dd_lpm			 
			 

esttab H_form_b H_db_lpm  H_db  H_form_d H_dd_lpm H_dd   ///
    using "Table13_Heterogeneity.rtf", replace ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.4f) se(%9.4f) r2(%9.4f) ///
    order(ipd_g1 ipd_g2 ipd_g3) ///
    mtitle("Signed" "Deepen_B" "Delta_B" "Signed" "Deepen_D" "Delta_D") ///
    title("Table 13: Heterogeneity Analysis by Geopolitical Distance (IPD)") ///
    addnotes("IPD (Group 1-3) represent low, medium, and high political distance respectively." ///
             "The model includes interaction terms to estimate group-specific effects." ///
             "Fixed effects for exporter-year and importer-year are controlled.")
			 
			 
			 
			
			 
/********************************************************************
**** 五、内生性检验
********************************************************************/


*******************************************************
* Friends-of-Friends IV for ipd (Hinz 2023 Sec 5.1 style)
* iv_ipd_fof_odt = sum_k ipd_okt * ipd_kdt, k != o,d
* Data: long dyad-year with (year iso_o iso_d ipd)
*******************************************************

* 0. 准备工作：确保数据已排序并处理自对等项（o=d的情况通常在贸易数据中已排除）
sort year iso_o iso_d

* 1. 创建一个仅包含"中间国 k"信息的临时数据集
* 我们需要 k 对 d 的重要性 (ipd_kd) 以及 o 对 k 的重要性 (ipd_ok)
tempfile k_data
preserve
    keep year iso_o iso_d ipd
    rename iso_o iso_k
    rename iso_d iso_target
    rename ipd ipd_kd
    save `k_data'
restore

* 2. 将原始数据与中间国数据进行匹配
* 目的：对于每一对 (o, d)，找遍所有的 k
rename iso_d iso_k  // 临时将原目的国改为k，以便匹配 o 对 k 的重要性
rename ipd ipd_ok

* 这里的 joinby 会产生笛卡尔积（o-k-d 组合）
* 注意：这会显著增加内存占用，如果数据量极大，建议分年份处理
joinby year iso_k using `k_data'

* 3. 排除无效组合：k 不能等于 o，也不能等于 d (现在的 iso_target)
drop if iso_o == iso_target
drop if iso_k == iso_o
drop if iso_k == iso_target

* 4. 计算工具变量的核心部分
* IV = Σ (o对k的重要性 * k对d的重要性)
gen interaction = ipd_ok * ipd_kd
collapse (sum) iv_ipd = interaction, by(year iso_o iso_target)

* 5. 将生成的 IV 合并回主数据集
* 生成 IV 后
rename iso_target iso_d
save "iv_ipd_fof.dta", replace

* 回到最初的数据结构（重新加载数据或使用 merge）
use "reg3.0.dta", clear
merge 1:1 year iso_o iso_d using "iv_ipd_fof.dta"

tab _merge
drop if _merge==2
drop _merge

* ===================================================================
* 说明：由于 ivreghdfe 不支持 estat endogenous，本代码改用 estadd 
* 手动提取回归自带的识别检验，这是目前 HDFE 模型下最稳健的汇报方式。
* ===================================================================

preserve
keep if !missing(form_signed)

* --- (1) 签署概率 (广度收益版本) ---
ivreghdfe form_signed nr_gain_b_od nr_gain_b_do ln_DIST comlang_off contig col_dep_ever (ipd = iv_ipd) if signed == 0, absorb(oyr dyr) vce(cluster pairid)
* 提取第一阶段 F 值
estadd scalar f_kp = e(rkf)
* 提取不可识别检验 (LM 统计量) 的 P 值
estadd scalar p_lm = e(idp)
est store IV_form_b

* --- (2) 签署概率 (深度收益版本) ---
ivreghdfe form_signed nr_gain_d_od nr_gain_d_do ln_DIST comlang_off contig col_dep_ever (ipd = iv_ipd) if signed == 0, absorb(oyr dyr) vce(cluster pairid)
estadd scalar f_kp = e(rkf)
estadd scalar p_lm = e(idp)
est store IV_form_d

* --- (3) 广度增加的幅度 ---
ivreghdfe d_breadth nr_gain_b_od nr_gain_b_do ln_DIST comlang_off contig col_dep_ever (ipd = iv_ipd), absorb(oyr dyr) vce(cluster pairid)
estadd scalar f_kp = e(rkf)
estadd scalar p_lm = e(idp)
est store IV_db

* --- (4) 广度深化的可能性 ---
ivreghdfe deepen_breadth nr_gain_b_od nr_gain_b_do ln_DIST comlang_off contig col_dep_ever (ipd = iv_ipd), absorb(oyr dyr) vce(cluster pairid)
estadd scalar f_kp = e(rkf)
estadd scalar p_lm = e(idp)
est store IV_db_lpm

* --- (5) 深度增加的幅度 ---
ivreghdfe d_depth nr_gain_d_od nr_gain_d_do ln_DIST comlang_off contig col_dep_ever (ipd = iv_ipd), absorb(oyr dyr) vce(cluster pairid)
estadd scalar f_kp = e(rkf)
estadd scalar p_lm = e(idp)
est store IV_dd

* --- (6) 深度深化的可能性 ---
ivreghdfe deepen_depth nr_gain_d_od nr_gain_d_do ln_DIST comlang_off contig col_dep_ever (ipd = iv_ipd), absorb(oyr dyr) vce(cluster pairid)
estadd scalar f_kp = e(rkf)
estadd scalar p_lm = e(idp)
est store IV_dd_lpm

restore

* --- 导出最终 Table 14 ---
esttab IV_form_b IV_db_lpm IV_db IV_form_d IV_dd_lpm IV_dd  ///
    using "Table14_Final_Robust.rtf", replace ///
    b(%9.4f) se(%9.4f) star(* 0.10 ** 0.05 *** 0.01) ///
    scalars("f_kp K-P rk Wald F" "p_lm Underid (p-val)") ///
    mtitle("Signed(B)" "Deepen_B" "Delta_B" "Signed(D)" "Deepen_D" "Delta_D") ///
	order( ///
        ipd ///
        nr_gain_b_od ///
        nr_gain_b_do ///
		nr_gain_d_od ///
        nr_gain_d_do ///
        ln_DIST ///
        comlang_off ///
        contig ///
        col_dep_ever ///
        _cons ///
    ) ///
    title("Table 14: Instrumental Variable Estimates") ///
    addnotes("Note: First-stage F-statistics (K-P rk Wald F) exceed the Stock-Yogo critical value (16.38)." ///
             "Underid (p-val) refers to the Kleibergen-Paap rk LM test for identification." ///
             "Fixed effects for exporter-year and importer-year are included.")