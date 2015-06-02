import rpy2
rpy2.__path__
import rpy2.robjects.lib.ggplot2 as ggplot2
import rpy2.robjects as robjects


d = {'a': robjects.IntVector((1,2,3)), 'b': robjects.IntVector((4,5,6))}

dataf = robject.DataFrame(d)


pp = ggplot2.ggplot(dataf) + \
     ggplot2.aes_string(x='a', y='b', col='factor(cyl)') + \
     ggplot2.geom_point() + \
     ggplot2.geom_smooth(ggplot2.aes_string(group = 'cyl'),
                         method = 'lm')
