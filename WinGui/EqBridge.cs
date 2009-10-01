using System;
using System.Collections.Generic;
using System.Linq;
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

        [DllImport("kernel32.dll")]
        private static extern IntPtr LoadLibrary(string fileName);

        [DllImport("kernel32.dll", CharSet = CharSet.Ansi, ExactSpelling = true, SetLastError = true)]
        public static extern UIntPtr GetProcAddress(IntPtr hModule, string procName);

        private delegate void ForeignAction();
        private delegate String Evaluator( String input );

        #region Dll imports
        [DllImport("formulaDll.dll", EntryPoint = "eq_begin_runtime", CallingConvention=CallingConvention.Cdecl)]
        private static extern bool InitRuntime();
        
        [DllImport("formulaDll.dll", EntryPoint = "eq_end_runtime")]
        private static extern void EndRuntime();
        
        [DllImport("formulaDll.dll", EntryPoint = "eq_cleanup_last_result")]
        private static extern void CleanupLastResult();
        
        [DllImport("formulaDll.dll", CharSet = CharSet.Unicode, EntryPoint = "eq_eval")]
        private static extern String Eval(String input);
        
        #endregion /* Dll imports */        
    
        private void Init()
        {
            if (alreadyUsed)
                throw new HaskellRuntimeException("Haskell subsystem has already been used");

            if (initialized) return;

            IntPtr formulaLib = LoadLibrary("formulaDll.dll");
            UIntPtr 
            
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
            if (!initialized) Init();

            String rez = Eval(program);
            CleanupLastResult();
            return rez;
        }
    }
}
