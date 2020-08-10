using ScottPlot;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.IO;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Globalization;
using System.Threading;

namespace coreFormsTest
{
    public partial class Form : System.Windows.Forms.Form
    {
        readonly string outPath = "../outfile.txt";
        FormsPlot formsPlot = new FormsPlot();

        public Form()
        {
            InitializeComponent();
        }
 
        private void Form1_Load(object sender, EventArgs e)
        {
            Thread.CurrentThread.CurrentCulture = new CultureInfo("en-GB");

            var rawData = File.ReadAllText(outPath).Split("\n");
            var rawPredator = rawData[0];
            var rawPrey = rawData[1];

            var predator = rawPredator.Split(" ").
                Where(x => !string.IsNullOrWhiteSpace(x)).
                Select(x => Convert.ToDouble(x)).ToArray();

            var prey = rawPrey.Split(" ").
                Where(x => !string.IsNullOrWhiteSpace(x)).
                Select(x => Convert.ToDouble(x)).ToArray();

            var enumeration = Enumerable.Range(0, prey.Length).
                Select(x => Convert.ToDouble(x)).
                ToArray();

            formsPlot.Dock = DockStyle.Fill;
            formsPlot.Location = new Point(0, 0);
            formsPlot.Name = "formsPlot1";
            this.Text = "Predator Prey Model Viewer";

            Plot plt = new Plot();

            plt.PlotScatter(enumeration, predator, label: "predator");
            plt.PlotScatter(enumeration, prey, label: "prey");
            plt.Legend();

            plt.Title("Predator Prey Model");
            plt.YLabel("Output");
            plt.XLabel("Boxes");

            formsPlot.Reset(plt);

            Controls.Add(formsPlot);
        }
    }
}
