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

            mathInput = new MathInputControl();
            mathInput.SetOwnerWindow(rezEntrySplitter.Panel1.Handle.ToInt32());
            mathInput.Show();
            mathInput.Insert += new _IMathInputControlEvents_InsertEventHandler(mathInput_Insert);
            mathInput.Close += new _IMathInputControlEvents_CloseEventHandler(mathInput_Close);

            computationKernel = kernel;
        }

        void mathInput_Close()
        {
            mathInput.Hide();
        }

        void mathInput_Insert(string RecoResult)
        {
            MessageBox.Show(RecoResult);
        }

        private void txtEntry_KeyUp(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter && e.Modifiers == Keys.Shift)
            {
                String result = computationKernel.EvalProgram(txtEntry.Text);
                txtEntry.Text = "";
                txtResult.AppendText("------------------------------\n");
                txtResult.AppendText(result.Replace("\n","\r\n"));
            }
        }
    }
}
