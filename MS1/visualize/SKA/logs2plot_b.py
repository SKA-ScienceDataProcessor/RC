__author__ = "mcsquaredjr"


import matplotlib
matplotlib.use('pdf')
matplotlib.rcParams['axes.linewidth'] = 0.5
import matplotlib.pyplot as plt
import readlog


RED = [0.77, 0.16, 0.31]
BLUE = [0.33, 0.63, 0.82]
GREEN = [0.1, 0.66, 0.53]
YELLOW = [0.93, 0.67, 0.34]
DARKBLUE = [0.27, 0.55, 0.78]
SEABLUE = [0.38, 0.75, 0.86]
MAGENTA = [0.79, 0.15, 0.98]
PALEMAGENTA = [0.6, 0.61, 0.99]


def all_times(evt_lst, zooming=False):
    fig = plt.figure()
    plt.hold("on")

    events = []

    for evt in evt_lst:
        miniset = set()
        if evt[0] == "collection phase":
            color = [0.5, 0.5, 0.5]
            shift = .1
            events.append(evt[0])

        elif evt[0] == "compute sends sum":
            color = PALEMAGENTA
            shift = .2
            events.append(evt[0])

        elif evt[0] == "generating and sending precomputed vector":
            color = MAGENTA
            shift = .3
            events.append(evt[0])

        elif evt[0] == "master waits for result":
            color = RED
            shift = .0
            events.append(evt[0])

        elif evt[0] == "pure computation time":
            color = BLUE
            shift = .4
            events.append(evt[0])

        elif evt[0] == "reading file":
            color = SEABLUE
            shift = .5
            events.append(evt[0])

        elif evt[0] == "receiving computed vector":
            color = [0, 0, 0]
            shift = .6
            events.append(evt[0])

        elif evt[0] == "receiving read vector":
            color = YELLOW
            shift = .7
            events.append(evt[0])

        else:
            color = GREEN
            shift = .8
            events.append(evt[0])



        if not zooming:
            if len(events) != len(set(events)):
                plt.plot([evt[1]/1e6, evt[2]/1e6], [evt[6] + 4*shift, evt[6] + 4*shift], "-", color=color, lw=1.5)
                #plt.plot([evt[1]/1e6, evt[1]/1e6 + 10*evt[3]], [evt[6] + 4*shift, evt[6] + 4*shift], "-", color=color, lw=1.5)
            else:
                if evt[0] == "generating and sending precomputed vector":
                    label = "generating and sending \nprecomputed vector"
                else:
                    label = evt[0]
                plt.plot([evt[1]/1e6, evt[2]/1e6], [evt[6] + 4*shift, evt[6] + 4*shift], "-", color=color, lw=1.5, label=label)
                #plt.plot([evt[1]/1e6, evt[1]/1e6 + 10*evt[3]], [evt[6] + 4*shift, evt[6] + 4*shift], "-", color=color, lw=1.5, label=label)
        else:
            ax = plt.axes([.65, .6, .2, .2], axisbg='y')
            if len(events) != len(set(events)):
                ax.plot([evt[1]/1e6, evt[2]/1e6], [evt[6] + 4*shift, evt[6] + 4*shift], "-", color=color, lw=1.5)
                #plt.plot([evt[1]/1e6, evt[1]/1e6 + 10*evt[3]], [evt[6] + 4*shift, evt[6] + 4*shift], "-", color=color, lw=1.5)
            else:
                if evt[0] == "generating and sending precomputed vector":
                    label = "generating and sending \nprecomputed vector"
                else:
                    label = evt[0]
                ax.plot([evt[1]/1e6, evt[2]/1e6], [evt[6] + 4*shift, evt[6] + 4*shift], "-", color=color, lw=1.5, label=label)


        plt.grid("on")


    plt.ylim([-1, 120])
    plt.xlabel("time, ms")
    plt.ylabel("node number")
    # plt.legend(('collection phase',
    #             'compute sends sum',
    #             'generating and sending \nprecomputed vector',
    #             'master waits for result',
    #             'pure computation time',
    #             'reading file',
    #             'receiving computed vector',
    #             'receiving read vector',
    #             'receiving vectors'), fontsize=9)
    plt.legend(fontsize=9, fancybox=True, shadow=True, ncol=1, loc='best')
    plt.title("Dot product profiling results, 48-node cluster")
    plt.show()

    fig.canvas.draw()
    fig.savefig("times.pdf")



if __name__ == "__main__":
    #evt_folder = "/Users/serge/Downloads/logs_48nodes"
    #evt_folder = "/Users/serge/Downloads/textualLogs_32nodes"
    evt_folder = "/Users/serge/Downloads/textualLogs_48nodes"
    ed = readlog.events_dict(evt_folder)
    print ed['events']
    all_times(ed['list'], True)