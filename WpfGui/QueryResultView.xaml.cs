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

namespace WpfGui
{
    /// <summary>
    /// Interaction logic for QueryResult.xaml
    /// </summary>
    public partial class QueryResultView : UserControl
    {
        MainWindow appWindow;

        public QueryResultView()
        {
            InitializeComponent();
        }

        public MainWindow AppWindow
        {
            set { appWindow = value; }
        }

        bool isFlagged = false;
        private void TextBox_MouseDown(object sender, MouseButtonEventArgs e)
            { isFlagged = true; }

        private void TextBox_MouseLeave(object sender, MouseEventArgs e)
            { isFlagged = false; }

        private void TextBox_MouseLeftButtonUp(object sender, MouseButtonEventArgs e)
        {
            if (isFlagged)
            {
                appWindow.AppendInput((string)((TextBox)sender).ToolTip);
                isFlagged = false;
            }
        }

        private void TextBox_MouseRightButtonUp(object sender, MouseButtonEventArgs e)
        {
        }
    }
}
