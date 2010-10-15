namespace WinGui
{
    partial class EqMainView
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.rezEntrySplitter = new System.Windows.Forms.SplitContainer();
            this.txtResult = new System.Windows.Forms.TextBox();
            this.txtEntry = new System.Windows.Forms.TextBox();
            this.rezEntrySplitter.Panel1.SuspendLayout();
            this.rezEntrySplitter.Panel2.SuspendLayout();
            this.rezEntrySplitter.SuspendLayout();
            this.SuspendLayout();
            // 
            // rezEntrySplitter
            // 
            this.rezEntrySplitter.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.rezEntrySplitter.Cursor = System.Windows.Forms.Cursors.Default;
            this.rezEntrySplitter.Dock = System.Windows.Forms.DockStyle.Fill;
            this.rezEntrySplitter.FixedPanel = System.Windows.Forms.FixedPanel.Panel2;
            this.rezEntrySplitter.Location = new System.Drawing.Point(0, 0);
            this.rezEntrySplitter.Name = "rezEntrySplitter";
            this.rezEntrySplitter.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // rezEntrySplitter.Panel1
            // 
            this.rezEntrySplitter.Panel1.Controls.Add(this.txtResult);
            // 
            // rezEntrySplitter.Panel2
            // 
            this.rezEntrySplitter.Panel2.Controls.Add(this.txtEntry);
            this.rezEntrySplitter.Size = new System.Drawing.Size(365, 555);
            this.rezEntrySplitter.SplitterDistance = 407;
            this.rezEntrySplitter.TabIndex = 0;
            // 
            // txtResult
            // 
            this.txtResult.BackColor = System.Drawing.SystemColors.Window;
            this.txtResult.Dock = System.Windows.Forms.DockStyle.Fill;
            this.txtResult.Font = new System.Drawing.Font("Lucida Console", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtResult.Location = new System.Drawing.Point(0, 0);
            this.txtResult.Multiline = true;
            this.txtResult.Name = "txtResult";
            this.txtResult.ReadOnly = true;
            this.txtResult.Size = new System.Drawing.Size(363, 405);
            this.txtResult.TabIndex = 0;
            // 
            // txtEntry
            // 
            this.txtEntry.Dock = System.Windows.Forms.DockStyle.Fill;
            this.txtEntry.Font = new System.Drawing.Font("Microsoft Sans Serif", 16F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtEntry.Location = new System.Drawing.Point(0, 0);
            this.txtEntry.Multiline = true;
            this.txtEntry.Name = "txtEntry";
            this.txtEntry.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.txtEntry.Size = new System.Drawing.Size(363, 142);
            this.txtEntry.TabIndex = 1;
            // 
            // EqMainView
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(365, 555);
            this.Controls.Add(this.rezEntrySplitter);
            this.Name = "EqMainView";
            this.Text = "Eq!";
            this.rezEntrySplitter.Panel1.ResumeLayout(false);
            this.rezEntrySplitter.Panel1.PerformLayout();
            this.rezEntrySplitter.Panel2.ResumeLayout(false);
            this.rezEntrySplitter.Panel2.PerformLayout();
            this.rezEntrySplitter.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.SplitContainer rezEntrySplitter;
        private System.Windows.Forms.TextBox txtResult;
        private System.Windows.Forms.TextBox txtEntry;
    }
}

