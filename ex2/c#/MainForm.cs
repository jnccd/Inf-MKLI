using ScottPlot;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace ex2
{
    public partial class MainForm : Form
    {
        FormsPlot formsPlot1 = new FormsPlot();

        public MainForm()
        {
            InitializeComponent();
            this.Load += new System.EventHandler(this.Form1_Load);

            formsPlot1.Dock = DockStyle.Fill;
            formsPlot1.Location = new Point(0, 0);
            formsPlot1.Name = "formsPlot1";

            Plot plt = new Plot();

            int pointCount = 51;
            double[] xs = DataGen.Consecutive(pointCount);
            double[] sin = DataGen.Sin(pointCount);
            double[] cos = DataGen.Cos(pointCount);

            plt.PlotScatter(xs, sin, label: "sin");
            plt.PlotScatter(xs, cos, label: "cos");
            plt.Legend();

            plt.Title("Scatter Plot Quickstart");
            plt.YLabel("Vertical Units");
            plt.XLabel("Horizontal Units");

            formsPlot1.Reset(plt);

            Controls.Add(formsPlot1);
        }

        private void Form1_Load(object sender, EventArgs e)
        {

        }
    }
}
