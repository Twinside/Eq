using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace WinGui
{
    public partial class EqMainView : Form
    {
        private EqBridge computationKernel;

        public EqMainView(EqBridge kernel)
        {
            InitializeComponent();
            computationKernel = kernel;
        }


        private void txtEntry_KeyUp(object sender, KeyEventArgs e)
        {
        }
    }
}
