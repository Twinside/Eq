using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;

namespace WinGui
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            EqBridge kernel = null;

            try { kernel = new EqBridge(); }
            catch (HaskellRuntimeException e)
            {
                MessageBox.Show( e.Message
                               , "Error in initilalisation"
                               , MessageBoxButtons.OK
                               , MessageBoxIcon.Error);
            }
            catch (DllNotFoundException)
            {
                MessageBox.Show( "The application is installed incorectly"
                               , "Missing an application part"
                               , MessageBoxButtons.OK
                               , MessageBoxIcon.Error
                               );
            }

            if (kernel != null)
            {
                Application.EnableVisualStyles();
                Application.SetCompatibleTextRenderingDefault(false);
                Application.Run(new EqMainView(kernel));
            }
        }
    }
}
