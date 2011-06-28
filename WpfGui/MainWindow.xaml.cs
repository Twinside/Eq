using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

using micautLib;
using System.Windows.Threading;

namespace WpfGui
{
    /// <summary>
    /// Interaction logic for Window1.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private WinGui.EqBridge computationKernel;
        private MathInputControl mathInput;

        public MainWindow()
        {
            computationKernel = new WinGui.EqBridge(true);
            InitializeComponent();
        }

        private void eqValidate_Click(object sender, RoutedEventArgs e)
        {
            string formated = computationKernel.FormatProgram(txtInput.Text);
            string result = computationKernel.EvalProgramWithContext(txtInput.Text);

            txtInput.Text = "";
            rchTxtResultView.AppendText("------------------------------\r");
            rchTxtResultView.AppendText(formated.Replace("\n","\r"));
            rchTxtResultView.AppendText("=>\r");
            rchTxtResultView.AppendText(result.Replace("\n","\r"));

            rchTxtResultView.ScrollToEnd();
        }

        private void btnShowMathDraw_Click(object sender, RoutedEventArgs e)
        {
            if (mathInput == null)
            {
                mathInput = new MathInputControl();
                //mathInput.SetOwnerWindow(rezEntrySplitter.Panel1.Handle.ToInt32());
                mathInput.Insert += new _IMathInputControlEvents_InsertEventHandler(mathInput_Insert);
                mathInput.Close += new _IMathInputControlEvents_CloseEventHandler(mathInput_Close);
            }
            mathInput.Show();
        }

        void mathInput_Close()
        {
            mathInput.Hide();
        }
        private delegate void MethodInvoker();
        void mathInput_Insert(string RecoResult)
        {
            string rez = computationKernel.TranslateMathMLToEq(RecoResult);
            Application.Current.Dispatcher.BeginInvoke(DispatcherPriority.Background
                                                      , (MethodInvoker)delegate() { txtInput.Text = rez; });
        }

        private void txtInput_KeyUp(object sender, KeyEventArgs e)
        { 
            if (e.Key == Key.Enter && Keyboard.IsKeyDown(Key.LeftShift))
                eqValidate_Click(null, null);
        }

        private void MenuItem_Click(object sender, RoutedEventArgs e)
            { txtInput.Text += (string)((MenuItem)sender).Header + "()"; }
    }
}
