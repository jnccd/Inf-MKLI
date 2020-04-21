using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;

namespace ex2
{
    class ODE
    {

        Func<double[], double, double> f;
        Func<Func<double, double, double>, double> phi;
        double[] aditionalParams;

        // static Tuple<double[], double[]> timeLoop(
        //     Func<Func<double, double, double>, double> phi, 
        //     Func<double, double, double> f
        //     )
        // {

        // }
    }
}
