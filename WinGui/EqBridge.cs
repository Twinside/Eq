using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace WinGui
{
    [global::System.Serializable]
    public class HaskellRuntimeException : Exception
    {
        public HaskellRuntimeException(string message) : base(message) { }
        protected HaskellRuntimeException(
          System.Runtime.Serialization.SerializationInfo info,
          System.Runtime.Serialization.StreamingContext context)
            : base(info, context) { }
    }

    public class EqBridge : IDisposable
    {
        /// <summary>
        /// Tell if the haskell subsystem has been initialized.
        /// </summary>
        private static bool initialized = false;

        /// <summary>
        /// The haskell subsystem cannot be re-initialised after a first
        /// initialisation, we use this to notify this fact.
        /// </summary>
        private static bool alreadyUsed = false;

        #region Dll imports
        
        [DllImport("kernel32.dll")]
        private static extern IntPtr LoadLibrary(string fileName);

        [DllImport("kernel32.dll", CharSet = CharSet.Ansi, ExactSpelling = true, SetLastError = true)]
        public static extern IntPtr GetProcAddress(IntPtr hModule, string procName);

        private delegate void ForeignAction();
        private delegate bool ForeignTruth();

        [return: MarshalAs(UnmanagedType.LPWStr)]
        private delegate String Evaluator([MarshalAs(UnmanagedType.LPWStr)] String input);
                
        private static ForeignTruth InitRuntime;
        private static ForeignAction EndRuntime;
        private static ForeignAction CleanupLastResult;
        private static Evaluator Eval;

        #endregion /* Dll imports */        
    
        public EqBridge()
        {
            Init();
        }

        private void Init()
        {
            if (alreadyUsed)
                throw new HaskellRuntimeException("Haskell subsystem has already been used");

            if (initialized) return;

            IntPtr formulaLib = LoadLibrary("formulaDll.dll");

            if (formulaLib == IntPtr.Zero)
                throw new DllNotFoundException("formulaDll.dll");

            IntPtr initRuntime = GetProcAddress(formulaLib, "eq_begin_runtime");
            IntPtr endRuntime = GetProcAddress(formulaLib, "eq_end_runtime");
            IntPtr cleanupLastResult = GetProcAddress(formulaLib, "eq_cleanup_last_result");
            IntPtr eqEval = GetProcAddress(formulaLib, "eq_eval");

            if (initRuntime == IntPtr.Zero || endRuntime == IntPtr.Zero || cleanupLastResult == IntPtr.Zero || eqEval == IntPtr.Zero)
                throw new BadImageFormatException("The dll doesn't export the good functions");

            InitRuntime = (ForeignTruth)Marshal.GetDelegateForFunctionPointer(initRuntime, typeof(ForeignTruth));
            EndRuntime = (ForeignAction)Marshal.GetDelegateForFunctionPointer(endRuntime, typeof(ForeignAction));
            CleanupLastResult = (ForeignAction)Marshal.GetDelegateForFunctionPointer(cleanupLastResult, typeof(ForeignAction));
            Eval = (Evaluator)Marshal.GetDelegateForFunctionPointer(eqEval, typeof(Evaluator));

            if (!initialized)
            {
                if (!InitRuntime())
                    throw new HaskellRuntimeException("Failed to initialize the Haskell core");
            }
        }

        public void Dispose()
        {
            if (initialized)
            {
                CleanupLastResult();
                EndRuntime();
                initialized = false;
            }
        }

        public String EvalProgram(String program)
        {
            String rez = Eval(program);
            CleanupLastResult();
            return rez;
        }
    }
}
