__author__ = 'Sam Brooke'
__email__ = "s.brooke14@imperial.ac.uk"

import os
import glob
import yaml
import itertools
import math as m
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
import rpy2.robjects.lib.ggplot2 as ggplot2
import csv
from cement.core import foundation, controller
from cement.utils import shell


class PlotterData:
    "Holds data and config parameters"

    def __init__(self):
        os.chdir(r'./plot_configs')

        configs = {}

        for dir_name in glob.glob("*.yml"):
            f = open(dir_name)
            yc = yaml.load(f.read())
            configs[yc['name']] = yc
            f.close()

        self.__choose_configs(configs)

    def set_parameters(self, name, description, p_script, o_script):
        self.parameters = {
            'name': name,
            'description': description,
            'process_script': p_script,
            'plot_script': o_script
        }

    def set_data(self, load_list):
        self.data = []

        for k in load_list:
            self.data.append(Data(load_list[k], k))

    def __choose_configs(self, configs):

        sp = shell.Prompt("Pick plot config", options = configs.keys(), numbered = True)
        choice = configs[sp.input]

        self.list = choice['load_list'].keys()
        self.set_parameters(choice['name'], choice['description'], choice['process_module'], choice['plot_module'])
        self.set_data(choice['load_list'])

    def scenario(self, s):
        if s in self.list:
            i = self.list.index(s)
            return self.data[i]

class Data:
    # Object to hold data from comma delimited CSV files

    def __init__(self, data_path, data_name):

        self.columns = []
        column_collect = []

        with open(data_path, 'rb') as csvfile:
            r1, r2 = itertools.tee(csv.reader(csvfile,delimiter=','))
            i = 0
            row_len = len(next(r1))

            for i in range(row_len):
                column_collect.append(list())

            for row in r2:
                for i in range(row_len):
                    column_collect[i].append(row[i])

            i = i+1

        self.name = data_name

        for c in column_collect:
            self.columns.append(DataCol(c))

    def list_columns(self):
        col_names = []
        for c in self.columns:
            col_names.append(c.name)

        return col_names

    def column(self, col_name):
        col_list = self.list_columns()
        if col_name in col_list:
            i = col_list.index(col_name)
            col = self.columns[i]
            return col.data


class DataCol:
    # Object to hold column data

    def __init__(self, raw_col):

        self.name = raw_col[0]
        self.data = raw_col[1:len(raw_col)]


class PlotterAppController(controller.CementBaseController):
    class Meta:
        label = 'base'
        description = 'Python CLI application to automate QS data plots'
        arguments = [
            ( ['-d', '--data'], dict(action='store', dest='data',
                      help='path to data file') )]


class PlotterApp(foundation.CementApp):

    class Meta:
        label = 'QS_Plotter'
        base_controller = PlotterAppController

app = PlotterApp()

app.setup()

app.run()

PD = PlotterData()

grdevices = importr('grDevices')
reshape = importr('reshape2')
stats = importr('stats', on_conflict="warn")

r = robjects.r

# Area vs. discharge --- Needs to be put into a proper process file

mean_annual_bqart = map(float, PD.scenario('mean_annual').column('Qw (m^3/s)'))
areas = map(float, PD.scenario('mean_annual').column('A (km^2)'))

ccsm4_max_bqart = map(float, PD.scenario('ccsm4_max_t').column('Qw (m^3/s)'))
ccsm4_min_bqart = map(float, PD.scenario('ccsm4_min_t').column('Qw (m^3/s)'))
mpi_max_bqart = map(float, PD.scenario('mpi_max_t').column('Qw (m^3/s)'))
mpi_min_bqart = map(float, PD.scenario('mpi_min_t').column('Qw (m^3/s)'))
miroc_max_bqart = map(float, PD.scenario('miroc_max_t').column('Qw (m^3/s)'))
miroc_min_bqart = map(float, PD.scenario('miroc_min_t').column('Qw (m^3/s)'))

max_bqart_lgm = []
for x in range(0, len(ccsm4_max_bqart)):
    max_bqart_lgm.append(max([ccsm4_max_bqart[x], miroc_max_bqart[x], mpi_max_bqart[x]]))

min_bqart_lgm = []
for x in range(0, len(ccsm4_max_bqart)):
    min_bqart_lgm.append(min([ccsm4_min_bqart[x], miroc_min_bqart[x], mpi_min_bqart[x]]))

lgm_mid = []
lgm_min = []
lgm_max = []

for x in range(0, len(min_bqart_lgm)):
		average = (max_bqart_lgm[x] + min_bqart_lgm[x])/2
		diff = max_bqart_lgm[x]-average
		lgm_mid.append(average)
		lgm_min.append(average-diff)
		lgm_max.append(average+diff)
		


d = {'discharge': robjects.FloatVector(lgm_mid), 'areas': robjects.FloatVector(areas), \
	'max':robjects.FloatVector(lgm_max), 'min':robjects.FloatVector(lgm_min)}

dat_frame = robjects.DataFrame(d)

d2 = {'discharge': robjects.FloatVector(mean_annual_bqart), 'areas': robjects.FloatVector(areas)}

dat_frame2 = robjects.DataFrame(d2)

# Lgm Coefs
lm = stats.lm("discharge ~ areas", data=dat_frame)

summary = r.summary(lm)

r_sq = summary.rx2('r.squared')
r_sq = str(+round(r_sq[0],2))
coefs = r.coef(lm)
print coefs
ic = coefs.rx2(1)
sl_lgm = coefs.rx2(2)
sl_lgm = str(round(sl_lgm[0],3))
r.confint(lm)

r_sq_lab_lgm = "R^{2}~"+r_sq

# Mean Annual Coefs
lm = stats.lm("discharge ~ areas", data=dat_frame2)

summary = r.summary(lm)

r_sq = summary.rx2('r.squared')
r_sq = str(+round(r_sq[0],2))
coefs = r.coef(lm)
print coefs
ic = coefs.rx2(1)
sl = coefs.rx2(2)
sl = str(round(sl[0],3))
r.confint(lm)

r_sq_lab = "R^{2}~"+r_sq


y_lab = r("expression(Discharge (m^{3}/s))")
x_lab = r("expression(Area (km^{2}))")
annotate1 = r('annotate("text", x = '+str(max(areas)-30)+', y = 0.5, color = "red", label = "Mean Annual", parse=FALSE)')
annotate2 = r('annotate("text", x = '+str(max(areas)-30)+', y = 0.42, label = "'+r_sq_lab+'", color = "red", parse=TRUE)')
annotate3 = r('annotate("text", x = '+str(max(areas)-30)+', y = 0.34, label = "slope~'+sl+'", color = "red", parse=TRUE)')

annotate4 = r('annotate("text", x = '+str(max(areas)-150)+', y = 0.7, color = "blue", label = "LGM", parse=FALSE)')
annotate5 = r('annotate("text", x = '+str(max(areas)-150)+', y = 0.6, color = "blue", label = "'+r_sq_lab_lgm+'", parse=TRUE)')
annotate6 = r('annotate("text", x = '+str(max(areas)-150)+', y = 0.5, color = "blue", label = "slope~'+sl_lgm+'", parse=TRUE)')

pp = ggplot2.ggplot(dat_frame) + \
    ggplot2.aes_string(y='discharge', x='areas') + \
    ggplot2.ggtitle('Area vs. Sediment Flux') + \
    ggplot2.scale_x_log10(x_lab) + \
    ggplot2.theme_bw() + \
    ggplot2.stat_smooth(method = "lm", formula = 'y ~ x') + \
    ggplot2.scale_y_log10(y_lab) + \
    annotate1 + \
    annotate2 + \
    annotate3 + \
    annotate4 + \
    annotate5 + \
    annotate6 + \
    ggplot2.geom_point(color='blue') + \
    ggplot2.geom_errorbar(ggplot2.aes_string(ymin='min',ymax='max'), data=dat_frame, width=.02, alpha=.3) + \
    ggplot2.geom_point(data=dat_frame2,color='red',show_guide='FALSE' ) + \
    ggplot2.stat_smooth(data=dat_frame2, method = "lm", formula = 'y ~ x', color='red')

grdevices = importr('grDevices')

grdevices.pdf(file="area_qs.pdf")
pp.plot()
grdevices.dev_off()