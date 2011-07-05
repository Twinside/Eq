using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace WpfGui
{
    class QueryResult
    {
        private string rawQuery;
        private string formatedQuery;
        private string formatedResult;
        private string rawResult;
        private string mathMlResult;

        public QueryResult(WinGui.EqBridge computationKernel, string query)
        {
            rawQuery = query;
            formatedQuery = computationKernel.FormatProgram(query).Replace("\n","\r");

            var rez = computationKernel.EvalProgramWithContext(rawQuery);
            formatedResult = rez.Item1.Replace("\n","\r");
            rawResult = rez.Item2;
            mathMlResult = rez.Item3;
        }

        public string RawQuery
        {
            get { return rawQuery; }
            set { rawQuery = value; }
        }

        public string FormatedQuery
        {
            get { return formatedQuery; } 
            set { formatedQuery = value; }
        }

        public string FormatedResult
        {
            get { return formatedResult; } 
            set { formatedResult = value; }
        }

        public string RawResult { get { return rawResult; } }
        public string MathMlResult { get { return mathMlResult; } }
    }
}
