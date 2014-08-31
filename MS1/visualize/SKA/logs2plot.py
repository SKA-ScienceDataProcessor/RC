__author__ = "mcsquaredjr"

import sys

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
REDDISH = [0.96, 0.38, 0.47]
MAGENTA = [0.79, 0.15, 0.98]
PALEMAGENTA = [0.6, 0.61, 0.99]

SHIFT = 0.1


def all_times(evt_lst, title, uevents, min_port=0, zooming=False):
    fig = plt.figure()
    plt.hold("on")

    events = []

    for evt in evt_lst:
        miniset = set()
        if evt[0] == "collection phase":
            color = [0.5, 0.5, 0.5]
            shift = SHIFT * 0
            events.append(evt[0])

        elif evt[0] == "compute sends sum":
            color = PALEMAGENTA
            shift = SHIFT * 1
            events.append(evt[0])

        elif evt[0] == "generating and sending precomputed vector":
            color = MAGENTA
            shift = SHIFT * 0
            events.append(evt[0])

        elif evt[0] == "master waits for result":
            color = RED
            shift = SHIFT * 0
            events.append(evt[0])

        elif evt[0] == "pure computation time":
            color = BLUE
            shift = SHIFT * 2
            events.append(evt[0])

        elif evt[0] == "reading file":
            color = REDDISH
            shift = SHIFT * 3
            events.append(evt[0])

        elif evt[0] == "receiving computed vector":
            color = [0, 0, 0]
            shift = SHIFT * 4
            events.append(evt[0])

        elif evt[0] == "receiving read vector":
            color = YELLOW
            shift = SHIFT * 5
            events.append(evt[0])

        else:
            color = GREEN
            shift = SHIFT * 6
            events.append(evt[0])



        if not zooming:
            timeScale = 1.0e9
            process_no = evt[8] - min_port + 1
            if len(events) != len(set(events)):
                if evt[0] == "collection phase":
                    # I don't understand why it escapes the normal flow
                    plt.plot([evt[1]/timeScale, evt[2]/timeScale], [process_no + 4*shift, process_no + 4*shift], "-", color=color, lw=1.5, label=evt[0])
                else:

                    plt.plot([evt[1]/timeScale, evt[2]/timeScale], [process_no + 4*shift, process_no + 4*shift], "-", color=color, lw=1.5)
            else:

                if evt[0] == "generating and sending precomputed vector":
                    label = "generating and sending \nprecomputed vector"
                else:
                    label = evt[0]
                plt.plot([evt[1]/timeScale, evt[2]/timeScale], [process_no + 4*shift, process_no + 4*shift], "-", color=color, lw=1.5, label=label)
        else:

            ax1 = fig.add_subplot(111)
            if len(events) != len(set(events)):
                ax1.plot([evt[1]/timeScale, evt[2]/timeScale], [process_no + 4*shift, process_no + 4*shift], "-", color=color, lw=1.5)

            else:
                if evt[0] == "generating and sending precomputed vector":
                    label = "generating and sending \nprecomputed vector"
                else:
                    label = evt[0]
                ax1.plot([evt[1]/timeScale, evt[2]/timeScale], [process_no + 4*shift, process_no + 4*shift], "-", color=color, lw=1.5, label=label)

            ax2 = plt.axes([.4, .2, .2, .18], axisbg='w')
            if len(events) != len(set(events)):
                ax2.plot([evt[1]/1e6, evt[2]/1e6], [process_no + 4*shift, process_no + 4*shift], "-", color=color, lw=1.5)

            else:
                if evt[0] == "generating and sending precomputed vector":
                    label = "generating and sending \nprecomputed vector"
                else:
                    label = evt[0]
                ax2.plot([evt[1]/1e6, evt[2]/1e6], [process_no + 4*shift, process_no + 4*shift], "-", color=color, lw=1.5, label=label)

            plt.setp(ax2, xticks=[], yticks=[])
            plt.setp(ax2, ylim=[58, 64])
            plt.setp(ax2, xlim=[69420, 69470])
            plt.setp(ax2, ylabel="", xlabel="")

        plt.grid("on")

    if not zooming:
        plt.ylim([0, 120])
        plt.xlabel("time, ms")
        plt.ylabel("process number")
        #plt.legend(fontsize=9, fancybox=True, shadow=True, ncol=2, bbox_to_anchor=(0.98, 0.68), loc='best')
        plt.legend(fontsize=8, fancybox=True, shadow=True, ncol=3, loc='best')

        plt.title(title)
    else:
        plt.axes(ax1)
        plt.ylim([0, 120])
        plt.xlabel("time, ms")
        plt.ylabel("node number")
        plt.legend(fontsize=9, fancybox=True, shadow=True, ncol=3, loc='best')
        plt.title(title)
        plt.grid("on")

    plt.show()

    fig.canvas.draw()
    fig.savefig("times.pdf")



def distribution_plot(evt_lst, min_port):
    fig = plt.figure(figsize=(10, 15))
    plt.hold("on")


    axes = []
    nrows = 2
    ncols = 4

    titles = ["compute sends sum",
              "generating and sending \nprecomputed vector",
              "pure computation time",
              "reading file",
              "receiving computed vector",
              "receiving read vector",
              "receiving vectors"]

    for i in range(1, nrows*ncols):
        ax = plt.subplot(ncols, nrows, i)
        plt.subplots_adjust(hspace=0.3, wspace=0.3)
        plt.grid("on")
        plt.hold("on")
        plt.title(titles[i-1])
        plt.ylabel("time, ms")
        plt.xlim([0, 102])
        plt.xlabel("process number")
        axes.append(ax)


    for evt in evt_lst:
        proc_num = evt[8] - min_port + 1
        if evt[0] == "collection phase":
            color = [0.5, 0.5, 0.5]


        elif evt[0] == "compute sends sum":
            color = PALEMAGENTA
            plt.axes(axes[0])
            plt.bar(proc_num, evt[3], color=color)


        elif evt[0] == "generating and sending precomputed vector":
            color = MAGENTA
            plt.axes(axes[1])
            plt.bar(proc_num, evt[3], color=color)

        elif evt[0] == "master waits for result":
            color = RED


        elif evt[0] == "pure computation time":
            color = BLUE
            plt.axes(axes[2])
            plt.bar(proc_num, evt[3], color=color)

        elif evt[0] == "reading file":
            color = REDDISH
            plt.axes(axes[3])
            plt.bar(proc_num, evt[3], color=color)

        elif evt[0] == "receiving computed vector":
            color = [0, 0, 0]
            plt.axes(axes[4])
            plt.bar(proc_num, evt[3], color=color)

        elif evt[0] == "receiving read vector":
            color = YELLOW
            plt.axes(axes[5])
            plt.bar(proc_num, evt[3], color=color)

        else:
            color = GREEN
            plt.axes(axes[6])
            plt.bar(proc_num, evt[3], color=color)


    #fig.tight_layout()
    fig.canvas.draw()

    fig.savefig("distribution.pdf")

def compute_io_bandwidth(evt_lst, file_size):
    """Compute io bandwidth for dot product.
    """
    res = []
    for evt in evt_lst:
        if evt[0] == "reading file":
            duration = (evt[2] - evt[1]) * 1.0e-9
            bandwidth = file_size / duration / 1000 #GB
            res.append(bandwidth)
    return res, sum(res)



def compute_flops(evt_lst, batch_size, num_nodes):
    """Compute performance for dot product.
    """
    res_sum = []
    res_pure = []

    def _flops(batch_size, num_nodes, duration):
        return batch_size / (num_nodes - 2) * 2.0 / ((evt[2] - evt[1]) * 1.0e-9) / 1e9 #GFLOPS

    for evt in evt_lst:
        if evt[0] == "compute sends sum":
            res_sum.append(_flops(batch_size, num_nodes, evt[3]))
        if evt[0] == "pure computation time":
            res_pure.append(_flops(batch_size, num_nodes, evt[3]))

    return (res_sum, sum(res_sum), res_pure, sum(res_pure))



def performance_plot(evt_lst, file_size, batch_size, num_nodes):
    """Visualize bandwidth and performance.
    """

    # Compute stuff first
    b, sumb = compute_io_bandwidth(ed["list"], file_size)
    rs, srs, rp, srp = compute_flops(ed["list"], batch_size, num_nodes)

    # Bandwidth
    fig = plt.figure(figsize=(10/1.1, 12/1.1))
    ax1 = plt.subplot(311)
    plt.title("I/O bandwidth in {}-node cluster. Total I/O is {:.2f} GB/s\n".format(num_nodes, sumb))
    plt.xlabel("process number")
    plt.ylabel("GB/s")
    x = range(len(b))
    plt.bar(x, b, color=REDDISH)
    plt.xlim([-1, len(b) + 1])

    # Peformance, compute sends sum
    ax2 = plt.subplot(312)
    plt.title(' {}-node performance, "compute sends sum". Total: {:.2f} GFLOPS\n'.format(num_nodes, srs))
    plt.xlabel("process number")
    plt.ylabel("GFLOPS")
    x = range(len(rs))
    plt.bar(x, rs, color=GREEN)
    plt.xlim([-1, len(b) + 1])

    # Peformance, pure computation time
    ax3 = plt.subplot(313)
    plt.subplots_adjust(hspace=0.5)
    plt.title(' {}-node performance, "pure computation time". Total: {:.2f} GFLOPS\n'.format(num_nodes, srp))
    plt.xlabel("process number")
    plt.ylabel("GFLOPS")
    x = range(len(rp))
    plt.bar(x, rp, color=DARKBLUE)
    plt.xlim([-1, len(b) + 1])



    fig.canvas.draw()
    fig.savefig("performance-{}.pdf".format(num_nodes))


if __name__ == "__main__":
    #evt_folder = "/Users/serge/Downloads/logs_48nodes"
    #evt_folder = "/Users/serge/Downloads/textualLogs_32nodes"
    #evt_folder = "/Users/serge/Downloads/textualLogs_48nodes"
    #evt_folder = "/Users/serge/Downloads/textualLogs_64nodes"
    #evt_folder = "/Users/serge/Downloads/logs16node-N12_26_08_14"
    evt_folder = "/Users/serge/Downloads/textualLogs_100nodes_28_08_2014"
    if len(sys.argv) > 1:
        evt_folder = sys.argv[1]
    ed = readlog.events_dict(evt_folder)
    print "Unique events:", ed['events']
    all_times(ed['list'], "Dot product profiling results, 100-node cluster",
              ed["events"], min_port=ed["min_port"], zooming=False)
    distribution_plot(ed['list'], ed["min_port"])

    b, sumb = compute_io_bandwidth(ed["list"], 156)

    print "Bandwidth: ", b
    print "Total: ", sumb

    rs, srs, rp, srp = compute_flops(ed["list"], 20447232, 100)

    print "*** Event: Compute Sends Sum ***"
    print "per node: ", rs
    print "total: ", srs
    print "*** Event: Pure Computation Time ***"
    print "per node: ", rp
    print "total: ", srp

    #  You will need to change he last argument if you have a different number of nodes in the cluster!
    performance_plot(ed["list"], 156, 20447232, 100)