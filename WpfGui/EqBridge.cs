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

        private static int haskellRuntimeUseCount = 0;

        private IntPtr contextHandle;

        public EqBridge()
        {
            haskellRuntimeUseCount++;
            Init();
            contextHandle = dllCreateContext();
        }

        [DllImport("formulaDll.dll", EntryPoint="eq_begin_runtime", CallingConvention=CallingConvention.Cdecl)]
        private static extern int InitRuntime();

        [DllImport("formulaDll.dll", EntryPoint="eq_end_runtime", CallingConvention=CallingConvention.Cdecl)]
        private static extern void EndRuntime();

        [DllImport("formulaDll.dll", EntryPoint = "eq_evalW", CallingConvention = CallingConvention.Cdecl)]
        private static extern IntPtr dllCallEqEval([MarshalAs(UnmanagedType.LPWStr)]string txt);

        [DllImport("formulaDll.dll", EntryPoint = "eq_formatW", CallingConvention = CallingConvention.Cdecl)]
        private static extern IntPtr dllCallEqFormat([MarshalAs(UnmanagedType.LPWStr)]string txt);

        [DllImport("formulaDll.dll", EntryPoint = "eq_translate_mathml", CallingConvention = CallingConvention.Cdecl)]
        private static extern IntPtr dllCallMathMLToEq([MarshalAs(UnmanagedType.LPWStr)]string txt);

        [DllImport("formulaDll.dll", EntryPoint = "eq_create_context", CallingConvention=CallingConvention.Cdecl)]
        private static extern IntPtr dllCreateContext();

        [DllImport("formulaDll.dll", EntryPoint = "eq_delete_context", CallingConvention=CallingConvention.Cdecl)]
        private static extern void dllDeleteContext( IntPtr handle );

        [DllImport("formulaDll.dll", EntryPoint = "eq_eval_with_contextW", CallingConvention = CallingConvention.Cdecl)]
        private static extern IntPtr dllCallEqEvalWithContext([MarshalAs(UnmanagedType.LPWStr)]string txt, IntPtr context );

        public string EvalProgramWithContext(string txt) { return Marshal.PtrToStringUni(dllCallEqEvalWithContext(txt, contextHandle)); }
        public string EvalProgram(string txt) { return Marshal.PtrToStringUni(dllCallEqEval(txt)); }
        public string TranslateMathMLToEq(string txt) { return Marshal.PtrToStringUni(dllCallMathMLToEq(txt)); }
        public string FormatProgram(string txt) { return Marshal.PtrToStringUni(dllCallEqFormat(txt)); }

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
            dllDeleteContext(contextHandle);
            haskellRuntimeUseCount--;
            if (initialized && haskellRuntimeUseCount <= 0)
            {
                EndRuntime();
                initialized = false;
            }
        }
    }
}
