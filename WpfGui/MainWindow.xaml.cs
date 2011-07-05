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

        public void AppendInput(string txt)
            { txtInput.Text += txt; }

        private void eqValidate_Click(object sender, RoutedEventArgs e)
        {
            QueryResult rez = new QueryResult(computationKernel, txtInput.Text);
            QueryResultView view = new QueryResultView();
            view.DataContext = rez;
            view.AppWindow = this;
            panelResult.Children.Add(view);
            txtInput.Text = "";
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

        private delegate void MethodInvoker();

        void mathInput_Close() { mathInput.Hide(); }
        void mathInput_Insert(string RecoResult)
            { AppendInput(computationKernel.TranslateMathMLToEq(RecoResult)); }

        private void txtInput_KeyUp(object sender, KeyEventArgs e)
        {
            if (e.Key != Key.Enter) return;
            
            if (!Keyboard.IsKeyDown(Key.LeftShift))
                eqValidate_Click(null, null);
        }

        private void MenuItem_Click(object sender, RoutedEventArgs e)
            { txtInput.Text += (string)((MenuItem)sender).Header + "()"; }
    }
}
