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
    public partial class Form1 : Form
    {
        readonly string outPath = "../pp.txt";
        FormsPlot formsPlot1 = new FormsPlot();

        public Form1()
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
                Select(x => { 
                    try {
                        return Convert.ToDouble(x);
                    } catch {
                        return 0.0;
                    }
                }).ToArray();

            var prey = rawPrey.Split(" ").
                Where(x => !string.IsNullOrWhiteSpace(x)).
                Select(x => { 
                    try {
                        return Convert.ToDouble(x);
                    } catch {
                        return 0.0;
                    }
                }).ToArray();

            var enumeration = Enumerable.Range(0, prey.Length).
                Select(x => Convert.ToDouble(x)).
                ToArray();

            formsPlot1.Dock = DockStyle.Fill;
            formsPlot1.Location = new Point(0, 0);
            formsPlot1.Name = "formsPlot1";
            this.Text = "Predator Prey Model Viewer";

            Plot plt = new Plot();

            plt.PlotScatter(enumeration, predator, label: "predator");
            plt.PlotScatter(enumeration, prey, label: "prey");
            plt.Legend();

            plt.Title("Predator Prey Model");
            plt.YLabel("Output");
            plt.XLabel("Data Enumeration");

            formsPlot1.Reset(plt);

            Controls.Add(formsPlot1);
        }
    }
}
