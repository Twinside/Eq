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

        public EqBridge()
        {
            Init();
        }

        [DllImport("formulaDll.dll", EntryPoint="eq_begin_runtime", CallingConvention=CallingConvention.Cdecl)]
        private static extern int InitRuntime();

        [DllImport("formulaDll.dll", EntryPoint="eq_end_runtime", CallingConvention=CallingConvention.Cdecl)]
        private static extern void EndRuntime();

        [DllImport("formulaDll.dll", EntryPoint = "eq_evalW", CallingConvention = CallingConvention.Cdecl)]
        [return: MarshalAs(UnmanagedType.LPWStr)]
        private static extern string dllCallEqEval([MarshalAs(UnmanagedType.LPWStr)]string txt);

        [DllImport("formulaDll.dll", EntryPoint = "eq_translate_mathml", CallingConvention = CallingConvention.Cdecl)]
        [return: MarshalAs(UnmanagedType.LPWStr)]
        private static extern string dllCallMathMLToEq([MarshalAs(UnmanagedType.LPWStr)]string txt);

        public string EvalProgram(string txt)
        {
            if (!initialized) Init();
            return dllCallEqEval(txt);
        }

        public string TranslateMathMLToEq(string txt)
        {
            if (!initialized) Init();
            return dllCallMathMLToEq(txt);
        }

        private void Init()
        {
            if (alreadyUsed)
                throw new HaskellRuntimeException("Haskell subsystem has already been used");

            if (!initialized)
            {
                if (InitRuntime() == 0)
                    throw new HaskellRuntimeException("Failed to initialize the Haskell core");
            }
        }

        public void Dispose()
        {
            if (initialized)
            {
                EndRuntime();
                initialized = false;
            }
        }
    }
}
