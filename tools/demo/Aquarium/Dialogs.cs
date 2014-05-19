/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;

namespace Aquarium
{
    /// <summary>
    /// This is a commonly copied file which makes it easy to add file load
    /// and save dialogs to Avalon applications.  It does this using the
    /// standard Windows *FORMS* dialogs.  Therefore this code must be in 
    /// a separate file with a separate set of "using" directives at the top
    /// and you must also add System.Windows.Forms to the references for your
    /// project.
    /// </summary>
    static class Dialogs
    {
        /// <summary>
        /// Dialog box for user to select file to open.
        /// </summary>
        /// <param name="extension">Just the extension e.g. "foo"</param>
        /// <param name="filter">Description of filter e.g. "Foo files|*.foo"</param>
        /// <param name="filename">Out parameter filename chosen by user</param>
        /// <param name="addAll">Permit user to change to show *.*</param>
        /// <returns>true if user selected OK, false for Cancel</returns>
        public static bool OpenThese(string extension, string filter, 
            out string filename, bool addAll)
        {
            filename = null;
            OpenFileDialog dlg = new OpenFileDialog();
            if (addAll)
                filter = filter + "|All files|*.*";
            dlg.Filter = filter;
            dlg.DefaultExt = extension;
            dlg.Multiselect = false;
            dlg.CheckFileExists = true;
            dlg.SupportMultiDottedExtensions = true;
            DialogResult result = dlg.ShowDialog();
            if (result == DialogResult.OK)
            {
                filename = dlg.FileName;
                return true;
            }
            return false;
        }

        /// <summary>
        /// Dialog box for user to select file to save.
        /// </summary>
        /// <param name="extension">Just the extension e.g. "foo"</param>
        /// <param name="filter">Description of filter e.g. "Foo files|*.foo"</param>
        /// <param name="filename">Out parameter filename chosen by user</param>
        /// <param name="addAll">Permit user to change to show *.*</param>
        /// <returns>true if user selected OK, false for Cancel</returns>
        public static bool SaveThese(string extension, string filter,
            out string filename, bool addAll)
        {
            filename = null;
            SaveFileDialog dlg = new SaveFileDialog();
            if (addAll)
                filter = filter + "|All files|*.*";
            dlg.Filter = filter;
            dlg.DefaultExt = extension;
            dlg.CheckFileExists = false;
            dlg.SupportMultiDottedExtensions = true;
            DialogResult result = dlg.ShowDialog();
            if (result == DialogResult.OK)
            {
                filename = dlg.FileName;
                return true;
            }
            return false;
        }

        /// <summary>
        /// Dialog box for user to select file to open.  All also permitted.
        /// </summary>
        /// <param name="extension">Just the extension e.g. "foo"</param>
        /// <param name="filter">Description of filter e.g. "Foo files|*.foo"</param>
        /// <param name="filename">Out parameter filename chosen by user</param>
        /// <returns>true if user selected OK, false for Cancel</returns>
        public static bool OpenThese(string extension, string filter,
            out string filename)
        {
            return OpenThese(extension, filter, out filename, true);
        }


        /// <summary>
        /// Dialog box for user to select file to save.  All also permitted.
        /// </summary>
        /// <param name="extension">Just the extension e.g. "foo"</param>
        /// <param name="filter">Description of filter e.g. "Foo files|*.foo"</param>
        /// <param name="filename">Out parameter filename chosen by user</param>
        /// <returns>true if user selected OK, false for Cancel</returns>
        public static bool SaveThese(string extension, string filter,
            out string filename)
        {
            return SaveThese(extension, filter, out filename, true);
        }
    } // static class Dialogs
} // namespace
