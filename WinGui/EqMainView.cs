using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

using micautLib;

namespace WinGui
{
    public partial class EqMainView : Form
    {
        private EqBridge computationKernel;
        private MathInputControl mathInput;

        public EqMainView(EqBridge kernel)
        {
            InitializeComponent();

            computationKernel = kernel;
        }


        private void txtEntry_KeyUp(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter && e.Modifiers == Keys.Shift)
            {
                string formated = computationKernel.FormatProgram(txtEntry.Text);
                string result = computationKernel.EvalProgramWithContext(txtEntry.Text);
                
                txtEntry.Text = "";

                txtResult.AppendText("------------------------------\r\n");
                txtResult.AppendText(formated.Replace("\n","\r\n"));
                txtResult.AppendText("=>\r\n");
                txtResult.AppendText(result.Replace("\n","\r\n"));
            }
        }
    }
}
