using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;

using micautLib;

namespace MathMLDumper
{
    class MathInputDumper : ApplicationContext
    {
        private MathInputControl mathInput;
        private SaveFileDialog saveDlg;

        public MathInputDumper()
        {
            mathInput = new MathInputControl();
            mathInput.Show();
            mathInput.Insert += new _IMathInputControlEvents_InsertEventHandler(mathInput_Insert);
            mathInput.Close += new _IMathInputControlEvents_CloseEventHandler(mathInput_Close);
            saveDlg = new SaveFileDialog();
        }

        void mathInput_Close() { Application.Exit(); }

        void mathInput_Insert(string RecoResult)
        {
            if (saveDlg.ShowDialog() == DialogResult.OK)
                using (StreamWriter outFile = new StreamWriter( saveDlg.FileName
                                                              , false
                                                              , System.Text.Encoding.UTF32))
                    { outFile.Write(RecoResult); }
        }
    }

    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new MathInputDumper());
        }
    }
}
