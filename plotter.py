__author__ = 'Sam Brooke'
__email__ = "s.brooke14@imperial.ac.uk"

import os
import glob
import yaml
import itertools
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

r = robjects.r

bqart_qs = map(float, PD.scenario('ccsm4_max_t').column('Qs (m^3/yr)'))
areas = map(float, PD.scenario('ccsm4_max_t').column('A (km^2)'))

d = {'bqart_qs': robjects.FloatVector(bqart_qs), 'areas': robjects.FloatVector(areas)}

dat_frame = robjects.DataFrame(d)

pp = ggplot2.ggplot(dat_frame) + \
    ggplot2.aes_string(y='bqart_qs', x='areas') + \
    ggplot2.ggtitle('WWWOOWOOEWROEW') + \
    ggplot2.scale_x_continuous('A (km^2)') + \
    ggplot2.scale_y_continuous('Qs (m^3/yr)') + \
    ggplot2.geom_point()


grdevices = importr('grDevices')

grdevices.pdf(file="file.pdf")
pp.plot()
grdevices.dev_off()