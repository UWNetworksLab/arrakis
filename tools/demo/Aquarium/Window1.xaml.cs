/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

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
using System.Windows.Threading;

using System.IO;
using System.Globalization;
using System.Windows.Controls.Primitives;

using System.Net.Sockets;
using System.ComponentModel;
using System.Net; // IPAddress and Dns

namespace Aquarium
{
    /// <summary>
    /// Interaction logic for Window1.xaml
    /// </summary>
    public partial class Window1 : Window
    {
        public Window1()
        {
            InitializeComponent();

            App app = Application.Current as App;
            if (app != null)
            {
                // Add command line arguemnts to the connect menu
                foreach (string arg in app.CommandLineArguments)
                {
                    MenuItem mi = new MenuItem();
                    mi.Header = arg;
                    mi.Click += new RoutedEventHandler(this.Connect_Click);
                    this.menuitem_Connect.Items.Add(mi);
                }
            }

            this.Name = this.Title;
            Stream stream = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream("Aquarium.barrelfish.png");
            this.Icon = BitmapFrame.Create(stream);

            this.mainCanvas.MouseDown += new MouseButtonEventHandler(mainCanvas_MouseDown);

            this.worker = new BackgroundWorker();
            this.worker.DoWork += new DoWorkEventHandler(worker_DoWork);
            this.worker.RunWorkerCompleted += new RunWorkerCompletedEventHandler(worker_RunWorkerCompleted);
            this.worker.ProgressChanged += new ProgressChangedEventHandler(worker_ProgressChanged);
            this.Loaded += new RoutedEventHandler(Window1_Loaded);
            this.ContentRendered += new EventHandler(Window1_ContentRendered);
            this.StateChanged += new EventHandler(Window1_StateChanged);

            this.worker.WorkerReportsProgress = true;
            this.worker.WorkerSupportsCancellation = true;

            this.BlackPen = new Pen(Brushes.Black, 0.5);
            this.BlackPen.Freeze();

            this.VisHost = new VisualHost();

            this.VisRects = new MyDrawingVisual();
            this.VisThinRectangles = new MyDrawingVisual();
            this.VisBlobs = new MyDrawingVisual();
            this.VisArrows = new MyDrawingVisual();

            this.VisHost.AddVisual(this.VisRects);
            this.VisHost.AddVisual(this.VisThinRectangles);
            this.VisHost.AddVisual(this.VisBlobs);
            this.VisHost.AddVisual(this.VisArrows);
        }

        void Window1_ContentRendered(object sender, EventArgs e)
        {
            if (this.Automatic && !this.worker.IsBusy)
                this.worker.RunWorkerAsync(null);
        }

        void Window1_StateChanged(object sender, EventArgs e)
        {
            if (this.Automatic && !this.worker.IsBusy)
                this.worker.RunWorkerAsync(null);
        }

        void Window1_Loaded(object sender, RoutedEventArgs e)
        {
            ReRender();
        }

        // Barrelfish trace state
        private readonly BackgroundWorker worker;
        private string Filename;
        private bool Automatic;
        private TraceFetcher TraceFetcher;

        private TemporalTraceEventCollection ttec;
        private Dictionary<ulong, string> procnames;

        // Misc state

        private Dictionary<ulong, string> dcbstatic;

        // UI Data

        private Highlighting ClickHighlight = new Highlighting(SystemColors.HighlightBrush);
        private Highlighting HoverHighlight = new Highlighting(SystemColors.HotTrackBrush);


        private double PixelsPerCore = 20;
        private double NumCores = 16;
        //private double HalfCoreHeight = 8;

        private double TimeMin { get { return this.ttec == null ? 0.0 : this.ttec.MinTime; } }
        private double TimeMax { get { return this.ttec == null ? 1e6 : this.ttec.MaxTime; } }
        private double TimeRange { get { return TimeMax - TimeMin; } }

        private double TicksPerSecond = 1e9; // ???
        private double PixelsPerSecond;
        private double TicksPerPixel { get { return TicksPerSecond / PixelsPerSecond; } }
        private double PixelsPerTick { get { return PixelsPerSecond / TicksPerSecond; } }
        private double VisibleTimeMin;
        private double VisibleTimeMax;

        // UI State for the 'interactive' version of the trace visualisation
        // We have to do our own Z by ordering the appends
        private List<UIElement> Rectangles = new List<UIElement>();
        private List<UIElement> ThinRectangles = new List<UIElement>();
        private List<UIElement> Blobs = new List<UIElement>();
        private List<UIElement> Arrows = new List<UIElement>();

        // UI State for the 'passive' version of the trace visualization
        private Pen BlackPen;
        private VisualHost VisHost;
        private MyDrawingVisual VisRects;
        private MyDrawingVisual VisThinRectangles;
        private MyDrawingVisual VisBlobs;
        private MyDrawingVisual VisArrows;

        //
        // Various UI
        //

        void mainCanvas_MouseDown(object sender, MouseButtonEventArgs e)
        {
            if (e.ChangedButton != MouseButton.Left)
                return;
            if (sender != this.mainCanvas)
                return;
            FrameworkElement fe = e.Source as FrameworkElement;
            if (fe == null)
                return;
            if (fe == this.mainCanvas)
                this.ClickHighlight.Clear();
            else
            {
                this.HoverHighlight.Clear();
                this.ClickHighlight.Set(fe);
            }
        }

        void uie_MouseLeave(object sender, MouseEventArgs e)
        {
           this.HoverHighlight.Clear();
        }

        void uie_MouseEnter(object sender, MouseEventArgs e)
        {
            FrameworkElement fe = sender as FrameworkElement;
            this.HoverHighlight.Set(fe);

        }

        void mainCanvas_AddChild(UIElement uie)
        {
            if (this.optTooltips.IsChecked)
            {
                uie.MouseEnter += new MouseEventHandler(uie_MouseEnter);
                uie.MouseLeave += new MouseEventHandler(uie_MouseLeave);
            }
            else
            {
                uie.IsHitTestVisible = false;
            }
            this.mainCanvas.Children.Add(uie);
        }


        private void ShowBackground()
        {
            for (int i = 0; i < this.NumCores; i += 2)
            {
                Rectangle r = new Rectangle();
                r.Width = this.mainCanvas.Width;
                r.Height = PixelsPerCore;
                // Top left corner
                r.RenderTransform = new TranslateTransform(0, Y4Core(i)- PixelsPerCore/2.0);
                r.Fill = Brushes.LightGray;
                this.mainCanvas.Children.Add(r);
            }
        }

        //
        // Axes
        //

        private void ShowCpuAxis()
        {
            double xshift = (XScrollBar.Value - XScrollBar.Minimum) * PixelsPerTick;
            for (int i = 0; i < this.NumCores; i++)
            {
                TextBlock tb = new TextBlock();
                tb.Text = i.ToString();
                tb.RenderTransform = new TranslateTransform(xshift, Y4Core(i) - 5);
                this.mainCanvas.Children.Add(tb);
            }
        }


        private void ShowTimeAxis()
        {
            double ticks = this.TimeMax;
            double seconds = ticks / this.TicksPerSecond;
            long unit = 1;
            double ideal = TicksPerPixel * (dockPanel3.ActualWidth / 20.0);
            while (unit < ideal)
            {
                unit *= 10;
            }
            //unit = Math.Pow(10, Math.Floor(Math.Log10(ideal)));

            // Add a rectangle we can click on to see what's hapening
            //Rectangle r = Rect(Brushes.LightGray, 0, 0, seconds * PIXELSPERSECOND, 16);
            //r.ToolTip = "Left click for sampled profile info\nRight click to show running threads";
            //r.MouseLeftButtonDown += new MouseButtonEventHandler(timeline_MouseLeftButtonDown);
            //r.MouseRightButtonDown += new MouseButtonEventHandler(timeline_MouseRightButtonDown);
            //mainCanvas.Children.Add(r);

            // Number the time axis
            for (long t = 0; t < ticks; t += unit)
            {
                TextBlock tb = new TextBlock();
                tb.Text = String.Format("{0:n0}", t);
                tb.RenderTransform = new TranslateTransform(t / TicksPerPixel, 0);
                mainCanvas.Children.Add(tb);

                Line l = new Line();
                l.X1 = l.X2 = t / TicksPerPixel;
                l.Y1 = 14; // XXX
                l.Y2 = Y4Core(-1);
                l.StrokeThickness = 1.0;
                l.Stroke = Brushes.Black;
                this.mainCanvas.Children.Add(l);
            }
        }




        // In MSDN for FrameworkElement.VerticalAlignment:
        // "Canvas does not use VerticalAlignment when composing layout, 
        // because Canvas is based on absolute positioning"


        /// <summary>
        /// Returns the Y value of the middle of the line for the core
        /// </summary>
        double Y4Core(int core)
        {
            return (NumCores - core + 0.5) * PixelsPerCore + 0.5;
        }

        private SolidColorBrush BrushForName(string name)
        {
            if (string.IsNullOrEmpty(name))
                return Brushes.Plum;
            else if (name.Contains("(idle)"))
                return Brushes.DarkGray;
            else if (name.Contains("monitor"))
                return Brushes.CadetBlue;
            else if (name.Contains("mem_serv"))
                return Brushes.Red;
            else if (name.Contains("spantest"))
                return Brushes.Green;
            else if (name.Contains("chips"))
                return Brushes.Yellow;
            else if (name.Contains("e1000n"))
                return Brushes.BlueViolet;
            else if (name.Contains("serial"))
                return Brushes.OliveDrab;
            else if (name.Contains("bfscope"))
                return Brushes.Khaki;
            else if (name.Contains("fish"))
                return Brushes.GreenYellow;
            else if (name.Contains("pixels"))
                return Brushes.Red;
            else if (name.Contains("while1"))
                return Brushes.BlueViolet;
            else if (name.Contains("bomp_syn"))
                return Brushes.Red;
            else if (name.Contains("bomp_cpu"))
                return Brushes.Blue;
            else
                return Brushes.Green;
        }


        private void AddThinRectangle(int core, long start, long end, SolidColorBrush brush, string text)
        {
            if (this.optTooltips.IsChecked)
                AddInteractiveThinRectangle(core, start, end, brush, text);
            else
                AddVisThinRectangle(core, start, end, brush);
        }
        private void AddInteractiveThinRectangle(int core, long start, long end, SolidColorBrush brush, string text)
        {
            double x1 = start - this.TimeMin;
            double x2 = end - this.TimeMin;
            x1 /= this.TicksPerPixel;
            x2 /= this.TicksPerPixel;
            Rectangle r = new Rectangle();
            r.Width = x2 - x1;
            r.Height = 8;
            r.RenderTransform = new TranslateTransform(x1, Y4Core(core) - 4);
            r.Fill = brush;
            r.Stroke = Brushes.Black;
            if (this.optTooltips.IsChecked)
                r.ToolTip = text;
            r.SnapsToDevicePixels = true;
            this.ThinRectangles.Add(r);
        }
        private void AddVisThinRectangle(int core, long start, long end, SolidColorBrush brush)
        {
            double x1 = start - this.TimeMin;
            double x2 = end - this.TimeMin;
            x1 /= this.TicksPerPixel;
            x2 /= this.TicksPerPixel;
            if (x2 - x1 < 0.5) return;
            Rect r = new Rect(x1, Y4Core(core) - 4, x2 - x1, 8);
            this.VisThinRectangles.dc.DrawRectangle(brush, this.BlackPen, r);
        }

        private void AddRectangle(int core, long start, long end, SolidColorBrush brush, string text)
        {
            if (this.optTooltips.IsChecked)
                AddInteractiveRectangle(core, start, end, brush, text);
            else
                AddVisRectangle(core, start, end, brush);
        }
        private void AddInteractiveRectangle(int core, long start, long end, SolidColorBrush brush, string text)
        {
            double x1 = start - this.TimeMin;
            double x2 = end - this.TimeMin;
            x1 /= this.TicksPerPixel;
            x2 /= this.TicksPerPixel;

            if (x2 - x1 < 0.5) return;
            Rectangle r = new Rectangle();
            r.Width = x2 - x1;
            r.Height = 12;
            // Top left corner
            r.RenderTransform = new TranslateTransform(x1, Y4Core(core) - 6);
            r.Fill = brush;
            r.Stroke = Brushes.Black;
            if (this.optTooltips.IsChecked)
                r.ToolTip = text;
            r.SnapsToDevicePixels = true;
            this.Rectangles.Add(r);
        }
        private void AddVisRectangle(int core, long start, long end, SolidColorBrush brush)
        {
            double x1 = start - this.TimeMin;
            double x2 = end - this.TimeMin;
            x1 /= this.TicksPerPixel;
            x2 /= this.TicksPerPixel;
            if (x2 - x1 < 0.5) return;
            Rect r = new Rect(x1, Y4Core(core) - 6, x2 - x1, 12);
            this.VisRects.dc.DrawRectangle(brush, this.BlackPen, r);
        }

        private void AddLine(int sendcore, long start, int recvcore, long end, SolidColorBrush brush, string text)
        {
            if (this.optTooltips.IsChecked)
            {
                AddInteractiveLine(sendcore, start, recvcore, end, brush, text);
            }
            else
            {
                AddVisLine(sendcore, start, recvcore, end, brush);
            }
        }
        private void AddVisLine(int sendcore, long start, int recvcore, long end, SolidColorBrush brush)
        {
            double x1 = start - this.TimeMin;
            double x2 = end - this.TimeMin;
            x1 /= this.TicksPerPixel;
            x2 /= this.TicksPerPixel;
            this.VisArrows.dc.DrawLine(new Pen(brush, 1.0),
                    new Point(x1, Y4Core(sendcore)),
                    new Point(x2, Y4Core(recvcore)));
        }
        private void AddInteractiveLine(int sendcore, long start, int recvcore, long end, SolidColorBrush brush, string text)
        {
            double x1 = start - this.TimeMin;
            double x2 = end - this.TimeMin;
            x1 /= this.TicksPerPixel;
            x2 /= this.TicksPerPixel;
            Line l = new Line();
            l.X1 = x1;
            l.Y1 = Y4Core(sendcore);
            l.X2 = x2;
            l.Y2 = Y4Core(recvcore);
            l.StrokeThickness = 2.0;
            l.Stroke = brush;
            if (this.optTooltips.IsChecked)
                l.ToolTip = text;
            l.SnapsToDevicePixels = true;
            this.Arrows.Add(l);
        }

        private void AddArrow(int sendcore, long start, int recvcore, long end, SolidColorBrush brush, string text)
        {
            if (this.optTooltips.IsChecked)
            {
                AddInteractiveArrow(sendcore, start, recvcore, end, brush, text);
            }
            else
            {
                AddVisLine(sendcore, start, recvcore, end, brush);
            }
        }
        private void AddInteractiveArrow(int sendcore, long start, int recvcore, long end, SolidColorBrush brush, string text)
        {
            double x1 = ((double)start - this.TimeMin) / TicksPerPixel;
            double x2 = ((double)end - this.TimeMin) / TicksPerPixel;
            Point nock = new Point(x1, Y4Core(sendcore));
            Point tip = new Point(x2, Y4Core(recvcore));

            double mag;
            Vector unit, norm;
            MagNormUnit(nock, tip, out mag, out unit, out norm);

            Point left = tip - 5 * unit + 3 * norm;
            Point right = tip - 5 * unit - 3 * norm;

            Polygon pg = new Polygon();
            pg.Points = new PointCollection(5);
            pg.Points.Add(left);
            pg.Points.Add(tip);
            pg.Points.Add(nock);
            pg.Points.Add(tip);
            pg.Points.Add(right);
            pg.Points.Freeze();
            pg.Stroke = brush;
            pg.StrokeThickness = 1.0;
            pg.StrokeMiterLimit = 1.0;
            pg.Fill = brush;
            if (this.optTooltips.IsChecked)
                pg.ToolTip = text;
            pg.SnapsToDevicePixels = true;
            this.Arrows.Add(pg);
        }

        private static void MagNormUnit(Point one, Point two,
            out double mag, out Vector unit, out Vector norm)
        {
            double dx = two.X - one.X;
            double dy = two.Y - one.Y;
            mag = Math.Sqrt(dx * dx + dy * dy);
            if (mag < double.Epsilon)
                throw new DivideByZeroException("two points are co-located");
            unit = new Vector(dx / mag, dy / mag);
            norm = new Vector(-unit.Y, unit.X);
        }

        private void AddDiamond(int core, long time, SolidColorBrush brush, string text)
        {
            if (this.optTooltips.IsChecked)
                AddInteractiveDiamond(core, time, brush, text);
            else
                AddVisDiamond(core, time, brush);
        }
        private void AddInteractiveDiamond(int core, long time, SolidColorBrush brush, string text)
        {
            double x = ((double)time - this.TimeMin) / this.TicksPerPixel;
            double size = 8;
            Polygon dia = new Polygon();
            dia.Points = new PointCollection(4);
            dia.Points.Add(new Point(0, 0));
            dia.Points.Add(new Point(+size / 2, size / 2));
            dia.Points.Add(new Point(0, size));
            dia.Points.Add(new Point(-size / 2, size / 2));
            dia.Points.Freeze();
            dia.Fill = brush;
            dia.RenderTransform = new TranslateTransform(x, Y4Core(core));
            if (this.optTooltips.IsChecked)
                dia.ToolTip = text;
            dia.SnapsToDevicePixels = true;
            this.Blobs.Add(dia);
        }
        private void AddVisDiamond(int core, long time, SolidColorBrush brush)
        {
            double x = ((double)time - this.TimeMin) / this.TicksPerPixel;
            double y = Y4Core(core);
            double size = 8;
            StreamGeometry sg = new StreamGeometry();
            using (StreamGeometryContext sgc = sg.Open())
            {
                sgc.BeginFigure(new Point(x, y), true, true);
                sgc.LineTo(new Point(x + size / 2, y + size /2), true, false);
                sgc.LineTo(new Point(x + 0, y + size), true, false);
                sgc.LineTo(new Point(x - size / 2, y + size / 2), true, false);
            }
            sg.Freeze();
            this.VisBlobs.dc.DrawGeometry(brush, null, sg);
        }


        private void AddInteractiveBlob(int core, long time, SolidColorBrush brush, string text)
        {
            double x = ((double)time - this.TimeMin) / this.TicksPerPixel;
            Ellipse e = new Ellipse();
            e.Width = e.Height = 6;
            e.Fill = brush;
            if (this.optTooltips.IsChecked)
                e.ToolTip = text;
            e.RenderTransform = new TranslateTransform(x, Y4Core(core) - e.Height / 2);
            e.SnapsToDevicePixels = true;
            this.Blobs.Add(e);
        }
        private void AddVisBlob(int core, long time, SolidColorBrush brush, string text)
        {
            double x = ((double)time - this.TimeMin) / this.TicksPerPixel;
            this.VisBlobs.dc.DrawEllipse(brush, null, new Point(x, Y4Core(core)), 3, 3);
        }
        private void AddBlob(int core, long time, TraceEvent te)
        {
            if (this.optTooltips.IsChecked)
            {
                AddInteractiveBlob(core, time, Brushes.Black,
                    String.Format("At {0:n0}: ", time - this.TimeMin) + te.ToString());
            }
            else
            {
                AddVisBlob(core, time, Brushes.Black,
                    String.Format("At {0:n0}: ", time - this.TimeMin) + te.ToString());
            }
        }

        private void ReRender()
        {
            this.mainCanvas.Children.Clear();

            if (this.ttec != null && this.procnames != null)
            {
                ReRenderFromData(ttec, procnames);
            }
            else
            {
                Stream stream = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream("Aquarium.barrelfish.png");
                BitmapImage bi = new BitmapImage();
                bi.BeginInit();
                bi.StreamSource = stream;
                bi.EndInit();
                bi.Freeze();
                Image image = new Image();
                image.Source = bi;
                // Why do I need to do this?
                double width = this.dockPanel3.ActualWidth;
                double height = this.dockPanel3.ActualHeight;
                image.Width = width;
                image.Height = height;

                image.RenderTransform = new TranslateTransform(0, 0);

                this.mainCanvas.Children.Add(image);
                this.mainCanvas.Width = width;
                this.mainCanvas.Height = height;
            }
        }



        private struct ChannelAction : IComparable<ChannelAction>
        {
            public readonly long Time;
            public readonly int Core;
            public readonly ulong Channel;
            public readonly SolidColorBrush Colour;

            public ChannelAction(long time, int core, ulong channel, SolidColorBrush colour)
            {
                this.Time = time;
                this.Core = core;
                this.Channel = channel;
                this.Colour = colour;
            }

            public int CompareTo(ChannelAction other)
            {
                return this.Time.CompareTo(other.Time);
            }
        } // struct ChannelAction


        #region Event type constants

        private const ushort TRACE_SUBSYS_KERNEL = 0xFFFF;
        private const ushort TRACE_EVENT_CSWITCH = 0xCCCC;
        private const ushort TRACE_EVENT_BZERO   = 0xB0;
        private const ushort TRACE_EVENT_TIMERTICK = 0x1;
        private const ushort TRACE_EVENT_TIMER_SYNC = 0x2;

        private const ushort TRACE_EVENT_SCHED_MAKE_RUNNABLE = 0xED00;
        private const ushort TRACE_EVENT_SCHED_REMOVE = 0xED01;
        private const ushort TRACE_EVENT_SCHED_YIELD = 0xED02;
        private const ushort TRACE_EVENT_SCHED_SCHEDULE = 0xED03;
        private const ushort TRACE_EVENT_SCHED_CURRENT = 0xED04;

        private const ushort TRACE_EVENT_BMP_RX = 0xBEE1;
        private const ushort TRACE_EVENT_BMP_PRE_DELIVER = 0xBEE2;
        private const ushort TRACE_EVENT_BMP_POST_DELIVER = 0xBEE3;
        private const ushort TRACE_EVENT_BMP_PUMP = 0xBEE4;
        private const ushort TRACE_EVENT_BMP_SEND = 0xBEE5;


        private const ushort TRACE_SUBSYS_THREADS = 0xEEEE;
        private const ushort TRACE_EVENT_BARRIER_ENTER = 0x0100;
        private const ushort TRACE_EVENT_BARRIER_LEAVE = 0x0101;
        private const ushort TRACE_EVENT_MUTEX_LOCK_ENTER = 0x0200;
        private const ushort TRACE_EVENT_MUTEX_LOCK_LEAVE = 0x0201;
        private const ushort TRACE_EVENT_MUTEX_LOCK_NESTED_ENTER = 0x0202;
        private const ushort TRACE_EVENT_MUTEX_LOCK_NESTED_LEAVE = 0x0203;
        private const ushort TRACE_EVENT_MUTEX_TRYLOCK = 0x0204;
        private const ushort TRACE_EVENT_MUTEX_UNLOCK = 0x0205;

        private const ushort TRACE_EVENT_COND_WAIT_ENTER = 0x0300;
        private const ushort TRACE_EVENT_COND_WAIT_LEAVE = 0x0301;
        private const ushort TRACE_EVENT_COND_SIGNAL = 0x0302;
        private const ushort TRACE_EVENT_COND_BROADCAST = 0x0303;

        private const ushort TRACE_EVENT_SEM_WAIT_ENTER = 0x0400;
        private const ushort TRACE_EVENT_SEM_WAIT_LEAVE = 0x0401;
        private const ushort TRACE_EVENT_SEM_TRYWAIT = 0x0402;
        private const ushort TRACE_EVENT_SEM_POST = 0x0403;



        private const ushort TRACE_SUBSYS_MEMSERV = 0xA000;
        private const ushort TRACE_EVENT_ALLOC    = 0x0001;


        private const ushort TRACE_SUBSYS_MONITOR = 0xB000;
        private const ushort TRACE_EVENT_SPAN0    = 0x0000;
        private const ushort TRACE_EVENT_SPAN1    = 0x0001;
        private const ushort TRACE_EVENT_SPAN     = 0x0002;
        private const ushort TRACE_EVENT_PCREQ    = 0x0003;
        private const ushort TRACE_EVENT_PCREPLY  = 0x0004;
        private const ushort TRACE_EVENT_PCREQ_INTER   = 0x0005;
        private const ushort TRACE_EVENT_PCREPLY_INTER = 0x0006;
        private const ushort TRACE_EVENT_URPC_BLOCK   = 0x0007;
        private const ushort TRACE_EVENT_URPC_UNBLOCK = 0x0008;
        private const ushort TRACE_EVENT_REMOTE_CAP_RETYPE = 0x0009;
        private const ushort TRACE_EVENT_REMOTE_CAP_RETYPE_RETRY = 0x0010;
        private const ushort TRACE_EVENT_REMOTE_CAP_RETYPE_MSG = 0x0011;
        private const ushort TRACE_EVENT_REMOTE_CAP_RETYPE_END = 0x0012;
        private const ushort TRACE_EVENT_POLLING = 0xBBBB;
        

        private const ushort TRACE_SUBSYS_BFLIB = 0xBFBF;
        

        private const ushort TRACE_SUBSYS_CHIPS   = 0xC000;
        private const ushort TRACE_EVENT_CHIPS_LISTENCB = 0x0001;


        private const ushort TRACE_SUBSYS_TWEED       = 0x2000;
        private const ushort TRACE_EVENT_TWEED_START  = 0x0000;
        private const ushort TRACE_EVENT_TWEED_END    = 0x0001;
        private const ushort TRACE_EVENT_STEAL        = 0x0002;
        private const ushort TRACE_EVENT_STEAL_END    = 0x0003;
        private const ushort TRACE_EVENT_WAIT         = 0x0004;
        private const ushort TRACE_EVENT_WAIT_END     = 0x0005;
        private const ushort TRACE_EVENT_LOCKING      = 0x0006;
        private const ushort TRACE_EVENT_LOCKING_END  = 0x0007;


        private const ushort TRACE_SUBSYS_ROUTE = 0x3000;
        private const ushort TRACE_EVENT_BCAST_WITH_CCAST_SEND = 0x0001;
        private const ushort TRACE_EVENT_BCAST_WITH_CCAST = 0x0002;
        private const ushort TRACE_EVENT_RECV_BCAST_WITH_CCAST = 0x0003;
        private const ushort TRACE_EVENT_RECV_CCAST = 0x0004;
        private const ushort TRACE_EVENT_ROUTE_BENCH_START = 0x005;
        private const ushort TRACE_EVENT_ROUTE_BENCH_STOP = 0x0006;


        private const ushort TRACE_SUBSYS_BENCH = 0x1234;
        private const ushort TRACE_EVENT_PCBENCH = 0x0000;
        private const ushort TRACE_EVENT_RXPING = 0x0001;
        private const ushort TRACE_EVENT_RXPONG = 0x0002;


        private const ushort TRACE_SUBSYS_BOMP = 0x4000;
        private const ushort TRACE_EVENT_BOMP_START = 0x0001;
        private const ushort TRACE_EVENT_BOMP_STOP = 0x0002;
        private const ushort TRACE_EVENT_BOMP_ITER = 0x0003;

        #endregion

        private void ReadDataFromStream(StreamReader reader)
        {
            DateTime t0 = DateTime.Now;

            DateTime t1 = DateTime.Now;
            
            if (this.dcbstatic != null)
                this.procnames = new Dictionary<ulong,string>(this.dcbstatic);
            else
                this.procnames = new Dictionary<ulong,string>();

            this.procnames[0] = "(idle)";
            this.ttec = new TemporalTraceEventCollection();

            string line;
            while ((line = reader.ReadLine()) != null)
            {
                if (line.Length == 0)
                    continue;
                string[] tokens = line.Split();
                if (tokens.Length < 3) continue;

                if (tokens[0] == "#" && tokens[1] == "DCB" && tokens.Length == 5)
                {
                    ulong dcb;
                    if (ulong.TryParse(tokens[3], NumberStyles.AllowHexSpecifier, null, out dcb))
                    {
                        // 32-bit dcb with top bit set to identify not timestamp
                        if ((dcb & 0xffffffff00000000UL) == 0x8000000000000000UL)
                            dcb ^= 0x8000000000000000UL;
                        // We set rather than adding in case a duplicate happened somehow
                        procnames[dcb] = tokens[4];
                    }
                    continue;
                }
                if (tokens[0] == "#")
                    continue;

                if (!ttec.Add(tokens[0], tokens[1], tokens[2]))
                    continue;
            }
        }


        private string FromTo(string prepend, long from, long to)
        {
            string result = String.Format("from {0:n0} to {1:n0}",
                from - this.TimeMin, to - this.TimeMin);
            if (!string.IsNullOrEmpty(prepend))
                result = prepend + result;
            return result;
        }

        private void ReRenderFromData(TemporalTraceEventCollection ttec,
            Dictionary<ulong, string> procnames)
        {
            List<ChannelAction> channelactions = new List<ChannelAction>();
            bool heads = this.optHeads.IsChecked;

            if (this.optTooltips.IsChecked)
            {
                this.Rectangles.Clear();
                this.ThinRectangles.Clear();
                this.Blobs.Clear();
                this.Arrows.Clear();
            }
            else
            {
                this.VisRects.BeginRender();
                this.VisThinRectangles.BeginRender();
                this.VisBlobs.BeginRender();
                this.VisArrows.BeginRender();
            }

            for (int core = ttec.MinCore; core <= ttec.MaxCore; core++)
            {
                // TODO: XXX: This is x64 specific hack.  Also need to look
                // for both so we can continue to support the standard demo.
                const ulong OldHighBits = 0xffffff0000000000UL;
                const ulong NewHighBits = 0xffffff8000000000UL;

                long? bzero = null;
                long? runningstart = null;
                long? blockingstart = null;
                long? pollingstart = null;

                SolidColorBrush msgColour = null;

                long? tweedWorkStart = null;
                long? tweedWaitStart = null;
                long? tweedLockStart = null;
                bool isTweedSteal = false;

                string name = null;
                ulong? running = null;

                foreach (TemporalTraceEvent tte in ttec.OnCore(core))
                {
                    long now = tte.Time;
                    TraceEvent te = tte.Event;


                    //string text = tokens.Length >= 4 ? tokens[3] : null;
                    //if (string.IsNullOrEmpty(text) && dcb.HasValue)
                    //    procnames.TryGetValue(dcb.Value, out text);

                    // NB.  Take care when editing this code to not introduce
                    // serious performance regression.  This is a single if/elif/else
                    // where the last clause adds a "geneic" event.  Do NOT alter
                    // the flow control so that regular events also post a generic
                    // blob, or so that when a option is disabled that it posts
                    // a generic blob instead of the regular processing.

                    if (te.SubSys == TRACE_SUBSYS_KERNEL)
                    {
                        if (!optKernel.IsChecked) continue;

                        if (te.Event == TRACE_EVENT_BZERO)
                        {
                            if (te.Data == 1)
                                bzero = now;
                            else if (te.Data == 0 && bzero.HasValue)
                                AddThinRectangle(core, bzero.Value, now, Brushes.Purple,
                                    FromTo("bzero ", bzero.Value, now));
                            else
                                AddBlob(core, now, te);
                        }
                        else if (te.Event == TRACE_EVENT_CSWITCH)
                        {
                            if (running.HasValue && runningstart.HasValue)
                            {
                                if (!procnames.TryGetValue(running.Value, out name))
                                    if (!procnames.TryGetValue(running.Value | OldHighBits, out name))
                                        procnames.TryGetValue(running.Value | NewHighBits, out name);

                                AddRectangle(core, runningstart.Value, now, BrushForName(name),
                                    String.Format("{1}=0x{0:x} ", running.Value,
                                        string.IsNullOrEmpty(name) ? "?" : name)
                                        + FromTo(null, runningstart.Value, now));
                            }
                            runningstart = now;
                            running = te.Data;
                        }
                        else if (te.Event == TRACE_EVENT_TIMERTICK)
                        {
                            AddDiamond(core, now, Brushes.Black, "timer tick");
                        }
                        else if (te.Event == TRACE_EVENT_BMP_PUMP)
                        {
                            if (te.Data == 0)
                            {
                                if (pollingstart.HasValue)
                                    AddThinRectangle(core, pollingstart.Value, now, Brushes.Cyan,
                                        FromTo("Polling ", pollingstart.Value, now));
                                pollingstart = null;
                            }
                            else
                            {
                                pollingstart = now;
                            }
                        }
                        else
                            AddBlob(core, now, te);
                    }
                    else if (te.SubSys == TRACE_SUBSYS_MONITOR)
                    {
                        if (!optMonitor.IsChecked) continue;

                        if (te.Event == TRACE_EVENT_URPC_BLOCK)
                        {
                            // We just started blocking
                            if (runningstart.HasValue)
                                AddRectangle(core, runningstart.Value, now, BrushForName(name), name);
                            blockingstart = now;
                            //name = "monitor"; // XXX?
                            //running = null; // XXX?
                        }
                        else if (te.Event == TRACE_EVENT_URPC_UNBLOCK)
                        {
                            if (blockingstart.HasValue)
                                AddRectangle(core, blockingstart.Value, now, Brushes.DarkGray, "blocked");
                            else
                                AddBlob(core, now, te);
                        }
                        else if (te.Event == TRACE_EVENT_POLLING)
                        {
                            pollingstart = now;
                        }
                        else if (te.Event == TRACE_EVENT_REMOTE_CAP_RETYPE_MSG)
                        {
                            if (te.Data == 0)
                            {
                                msgColour = Brushes.Green;
                            }
                            else if (te.Data == 1)
                            {
                                msgColour = Brushes.IndianRed;
                            }
                            else if (te.Data == 3)
                            {
                                msgColour = Brushes.Red;
                            }
                        }
                        else
                            AddBlob(core, now, te);
                    }
                    else if (te.IsRecv)
                    {
                        channelactions.Add(new ChannelAction(now, core, te.Raw, null));
                    }
                    else if (te.IsSend)
                    {
                        channelactions.Add(new ChannelAction(now, core, te.Raw, msgColour));
                        msgColour = null;
                    }
                    else if (te.Raw == 0xBBBB)
                    {
                        // We just started blocking
                        if (runningstart.HasValue && running.HasValue)
                        {
                            if (!procnames.TryGetValue(running.Value, out name))
                                if (!procnames.TryGetValue(running.Value | OldHighBits, out name))
                                    procnames.TryGetValue(running.Value | NewHighBits, out name);

                            AddRectangle(core, runningstart.Value, now, BrushForName(name),
                                String.Format("{1}=0x{0:x} ", running.Value,
                                string.IsNullOrEmpty(name) ? "?" : name)
                                + FromTo(null, runningstart.Value, now));
                        }
                        blockingstart = now;
                    }
                    else if (te.Raw == 0xAAAA)
                    {
                        runningstart = now;
                    }
                    else if (te.Raw == 0xBBB)
                    {
                        pollingstart = now;
                    }
                    else if (te.Raw == 0xAAA)
                    {
                        if (pollingstart.HasValue)
                            AddThinRectangle(core, pollingstart.Value, now, Brushes.Cyan,
                                FromTo("Polling ", pollingstart.Value, now));
                        pollingstart = null;
                    }
                    else if (te.SubSys == TRACE_SUBSYS_MEMSERV)
                    {
                        if (!optAlloc.IsChecked) continue;

                        if (te.Event == TRACE_EVENT_ALLOC)
                            AddDiamond(core, now, Brushes.Navy, String.Format("Allocate {0}k",
                                1 << ((int)te.Data - 10)));
                        else
                            AddBlob(core, now, te);
                    }
                    else if (te.SubSys == TRACE_SUBSYS_TWEED)
                    {
                        if (!optTweed.IsChecked) continue;

                        if (te.Event == TRACE_EVENT_TWEED_START)
                        {
                            tweedWorkStart = now;
                            isTweedSteal = false;
                        }
                        else if (te.Event == TRACE_EVENT_STEAL)
                        {
                            if (tweedWaitStart.HasValue)
                            {
                                AddRectangle(core, tweedWaitStart.Value, now, Brushes.Purple, "Waiting");
                            }
                            if (optTweedArrows.IsChecked)
                            {
                                SolidColorBrush colour = tweedWaitStart.HasValue ? Brushes.Coral : Brushes.Orange;
                                if (heads)
                                    AddArrow((int)te.Data, now, core, now, colour,
                                          String.Format("Core {0} stealing tasks from core {1}",
                                          core, te.Data));
                                else
                                    AddLine((int)te.Data, now, core, now, colour,
                                        String.Format("Core {0} stealing tasks from core {1}",
                                        core, te.Data));
                            }
                            tweedWorkStart = now;
                            isTweedSteal = true;
                        }
                        else if (te.Event == TRACE_EVENT_STEAL_END)
                        {
                            SolidColorBrush colour = tweedWaitStart.HasValue ? Brushes.Coral : Brushes.Orange;
                            if (optTweedArrows.IsChecked)
                            {
                                if (heads)
                                    AddArrow(core, now, (int)te.Data, now, colour,
                                        String.Format("Core {0} completed tasks stolen from core {1}",
                                        core, te.Data));
                                else
                                    AddLine(core, now, (int)te.Data, now, colour,
                                        String.Format("Core {0} completed tasks stolen from core {1}",
                                        core, te.Data));
                            }
                            if (tweedWorkStart.HasValue)
                                AddRectangle(core, tweedWorkStart.Value, now, colour, "Stolen Tasks from core " + te.Data);
                            tweedWorkStart = null;
                            if (tweedWaitStart.HasValue)
                                tweedWaitStart = now;
                        }
                        else if (te.Event == TRACE_EVENT_TWEED_END)
                        {
                            if (tweedWorkStart.HasValue)
                                AddRectangle(core, tweedWorkStart.Value, now, Brushes.Yellow, "Main Task");
                            tweedWorkStart = null;
                        }
                        else if (te.Event == TRACE_EVENT_WAIT)
                        {
                            if (tweedWorkStart.HasValue)
                            {
                                SolidColorBrush colour = tweedWaitStart.HasValue ? Brushes.Coral : Brushes.Orange;
                                if (isTweedSteal)
                                    AddRectangle(core, tweedWorkStart.Value, now, colour, "Stolen Tasks");
                                else
                                    AddRectangle(core, tweedWorkStart.Value, now, Brushes.Yellow, "Main Task");
                            }

                            if (optTweedArrows.IsChecked)
                            {
                                if (heads)
                                    AddArrow(core, now, (int)te.Data, now, Brushes.DarkViolet,
                                        String.Format("Core {0} waiting for tasks stolen by core {1}",
                                        core, te.Data));
                                else
                                    AddLine(core, now, (int)te.Data, now, Brushes.DarkViolet,
                                        String.Format("Core {0} waiting for tasks stolen by core {1}",
                                        core, te.Data));
                            }
                            tweedWorkStart = null;
                            tweedWaitStart = now;
                        }
                        else if (te.Event == TRACE_EVENT_WAIT_END)
                        {
                            if (tweedWaitStart.HasValue)
                                AddRectangle(core, tweedWaitStart.Value, now, Brushes.DarkViolet, "Waiting for task stolen by core " + te.Data);

                            if (optTweedArrows.IsChecked)
                            {
                                if (heads)
                                    AddArrow((int)te.Data, now, core, now, Brushes.DarkViolet,
                                        String.Format("Core {0} can now continue, tasks stolen by core {1} completed",
                                        core, te.Data));
                                else
                                    AddLine((int)te.Data, now, core, now, Brushes.DarkViolet,
                                        String.Format("Core {0} can now continue, tasks stolen by core {1} completed",
                                        core, te.Data));
                            }
                            tweedWaitStart = null;
                            tweedWorkStart = now;
                        }
                        else if (te.Event == TRACE_EVENT_LOCKING)
                        {
                            tweedLockStart = now;
                        }
                        else if (te.Event == TRACE_EVENT_LOCKING_END)
                        {
                            if (tweedLockStart.HasValue)
                                AddRectangle(core, tweedLockStart.Value, now, Brushes.Maroon, "Waiting for Lock to be released");
                            tweedLockStart = null;
                        }
                        else
                        {
                            AddBlob(core, now, te);
                        }
                    } // tweed
                    else // An unknown type of event
                    {
                        if (optGeneric.IsChecked)
                        {
                            AddBlob(core, now, te);
                        }
                    }
                } // end foreach

                // End of data for this core,flush running process
                if (running.HasValue && runningstart.HasValue)
                {
                    if (!procnames.TryGetValue(running.Value, out name))
                        if (!procnames.TryGetValue(running.Value | OldHighBits, out name))
                            procnames.TryGetValue(running.Value | NewHighBits, out name);

                    if (!"monitor".Equals(name)) // test this way round in case name==null
                    {
                        AddRectangle(core, runningstart.Value, (long)this.TimeMax, BrushForName(name),
                        String.Format("{2}={0:x} from {1:n0} to ...",
                        running.Value, runningstart.Value - this.TimeMin,
                        string.IsNullOrEmpty(name) ? "?" : name));
                    }
                }
            } // end for core

            
            //Dictionary<ulong, KeyValuePair<int, long>> sent = new Dictionary<ulong, KeyValuePair<int, long>>();

            Dictionary<ulong, ChannelAction> sent = new Dictionary<ulong, ChannelAction>();
            Dictionary<ulong, ChannelAction> rcvd = new Dictionary<ulong, ChannelAction>();
            ulong rxbit = 0x0100000000000000;
            channelactions.Sort();
            if (optChannel.IsChecked)
            {
                foreach (ChannelAction ca in channelactions)
                {
                    // In theory EA is send and EB is recv, but sometimes clocks are funny
                    ulong chan = ca.Channel & 0x00FFFFFFFFFFFFFF;
                    if ((ca.Channel & rxbit) != 0)
                    {
                        ChannelAction other;
                        if (sent.TryGetValue(chan, out other))
                        {
                            sent.Remove(chan);
                            // Forwards in time
                            if (heads)
                                AddArrow(other.Core, other.Time, ca.Core, ca.Time, other.Colour ?? Brushes.Orange,
                                    String.Format("Chan={0:x} Sent {1:n0} Rcvd {2:n0}",
                                    chan, other.Time - this.TimeMin, ca.Time - this.TimeMin));
                            else
                                AddLine(other.Core, other.Time, ca.Core, ca.Time, other.Colour ?? Brushes.Orange,
                                    String.Format("Chan={0:x} Sent {1:n0} Rcvd {2:n0}",
                                    chan, other.Time - this.TimeMin, ca.Time - this.TimeMin));
                        }
                        else
                            rcvd.Add(chan, ca);
                    }
                    else
                    {
                        ChannelAction other;
                        if (rcvd.TryGetValue(chan, out other))
                        {
                            rcvd.Remove(chan);
                            // Bacwards in time
                            if (heads)
                                AddArrow(other.Core, other.Time, ca.Core, ca.Time, ca.Colour ?? Brushes.OrangeRed,
                                    String.Format("Chan={0:x} Sent {1:n0} Rcvd {2:n0}",
                                    chan, ca.Time - this.TimeMin, other.Time - this.TimeMin));
                            else
                                AddLine(other.Core, other.Time, ca.Core, ca.Time, ca.Colour ?? Brushes.OrangeRed,
                                    String.Format("Chan={0:x} Sent {1:n0} Rcvd {2:n0}",
                                    chan, ca.Time - this.TimeMin, other.Time - this.TimeMin));
                        }
                        else
                            sent.Add(chan, ca);
                    }
                }
            }

            DateTime t2 = DateTime.Now;

            // Any left over?
            if (optGeneric.IsChecked)
            {
                foreach (KeyValuePair<ulong, ChannelAction> kvp in sent)
                {
                    AddDiamond(kvp.Value.Core, kvp.Value.Time, Brushes.DarkTurquoise, kvp.Key.ToString("x"));
                }
                foreach (KeyValuePair<ulong, ChannelAction> kvp in rcvd)
                {
                    AddDiamond(kvp.Value.Core, kvp.Value.Time, Brushes.SeaGreen, kvp.Key.ToString("x"));
                }
            }
            DateTime t3 = DateTime.Now;
            
            // So they all have the right Z order
            ShowBackground();
            ShowTimeAxis();

            if (this.optTooltips.IsChecked == false)
            {
                this.VisRects.EndRender();
                this.VisThinRectangles.EndRender();
                this.VisBlobs.EndRender();
                this.VisArrows.EndRender();
                mainCanvas_AddChild(this.VisHost);
            }
            else
            {
                foreach (UIElement uie in this.Rectangles)
                    mainCanvas_AddChild(uie);
                foreach (UIElement uie in this.ThinRectangles)
                    mainCanvas_AddChild(uie);
                foreach (UIElement uie in this.Blobs)
                    mainCanvas_AddChild(uie);
                foreach (UIElement uie in this.Arrows)
                    mainCanvas_AddChild(uie);
            }
            ShowCpuAxis();
            
            DateTime t4 = DateTime.Now;
            
            // Otherwise last Blob is clipped at its LEFT edge, doh!
            // XXX This still doesnt quite work when zoomed in
            mainCanvas.Width += 10;
            // Stretch it out
            if (mainCanvas.Width < this.dockPanel3.ActualWidth)
                mainCanvas.Width = this.dockPanel3.ActualWidth;

            Console.WriteLine("Render {0} {1}", t3 - t2, t4 - t3);

//            System.Threading.Thread.Sleep(20); // XXX RI TMP

        } // Method RenderFromData



        // -------------------------

        public void SetZoom()
        {
            this.PixelsPerSecond = Math.Pow(2, Zoom.Value);

            double xsize = this.TimeRange * PixelsPerTick;
            mainCanvas.Width = xsize;
            //double xscale = PIXELSPERSECOND / (App.state.LastTs - App.state.FirstTs);
            Console.WriteLine("Canvas: {0} {1} {2}", mainCanvas.ActualWidth, mainCanvas.Width, xsize);

            // Scrollbar is in timestamp units ... it controls the timestamp value of the left 
            // hand edge of the main canvas window so it's value ranges from the first timestamp 
            // to the last timestamp minus the viewportsize.  The "viewport"
            // size tells the scrollbar how much of the range is visible.
            double visibleWidth = this.dockPanel3.ActualWidth;
            XScrollBar.Minimum = this.TimeMin;
            XScrollBar.ViewportSize = visibleWidth * TicksPerPixel;
            XScrollBar.Maximum = this.TimeMax - XScrollBar.ViewportSize;
            Console.WriteLine("ViewportSize {0}", XScrollBar.ViewportSize);

            // The 
            XScrollBar.LargeChange = XScrollBar.ViewportSize;
            XScrollBar.SmallChange = XScrollBar.ViewportSize / 10;
        } // SetZoom

        public void SetScroll()
        {
            double xshift = (XScrollBar.Value - XScrollBar.Minimum) * PixelsPerTick;
            this.VisibleTimeMin = XScrollBar.Value;
            this.VisibleTimeMax = this.VisibleTimeMin + XScrollBar.ViewportSize;
            Console.WriteLine("Scroll: visible range {0:f0}-{1:f0}", VisibleTimeMin, VisibleTimeMax);
            mainCanvas.RenderTransform = new TranslateTransform(-xshift, 0);
        } // SetScroll

        private void ConsiderRescale()
        {
            if (this.TimeRange / this.TicksPerPixel < this.dockPanel3.ActualWidth / 2)
            {
                double targetPixelsPerSecond = this.dockPanel3.ActualWidth
                    / (this.TimeRange / this.TicksPerSecond);
                this.Zoom.Value = Math.Floor(Math.Log(targetPixelsPerSecond, 2));
                SetZoom();
                SetScroll();
                ReRender();
            }
        } // ConsiderRescale

        // Event handlers

        private void FileDcb_Click(object sender, RoutedEventArgs e)
        {
            string filename;
            if (!Dialogs.OpenThese("bfl", "Barrelfish log files|*.bfl", out filename))
                return;

            this.dcbstatic = new Dictionary<ulong, string>();

            using (TextReader reader = new StreamReader(filename))
            {
                string line;
                while ((line = reader.ReadLine()) != null)
                {
                    if (string.IsNullOrEmpty(line) || line[0] == '#')
                        continue;

                    string[] tokens = line.Split();
                    ulong dcb;
                    // bool ok = ulong.TryParse(s, NumberStyles.AllowHexSpecifier, null, out raw);

                    if (tokens.Length == 3
                        && tokens[0] == "DCB:"
                        && tokens[1].StartsWith("0x")
                        && ulong.TryParse(tokens[1].Substring(2), NumberStyles.AllowHexSpecifier, null, out dcb))
                        this.dcbstatic.Add(dcb, tokens[2]);
                }
            }
            this.ReRender();
        } // FileDcb_Click

        private void FileOpen_Click(object sender, RoutedEventArgs e)
        {
            this.worker.CancelAsync();
            // XXX Cancel background worker
            if (this.TraceFetcher != null)
                this.TraceFetcher.Dispose();
            this.TraceFetcher = null;

            string filename;
            //if (!ViewEtl.Dialogs.OpenThese("txt", "Text files|*.txt", out filename))
            if (!Dialogs.OpenThese("bfl", "Barrelfish Log files|*.bfl", out filename))
                    return;

            this.Title = this.Name + ": " + filename;

            StreamReader reader = new StreamReader(filename);
            ReadDataFromStream(reader);
            reader.Close();

            this.Filename = filename;
            this.ReRender();
            this.zoomOut();
        } // FileOpen_Click


        private void Option_Click(object sender, RoutedEventArgs e)
        {
            ReRender();
        }

        private void Connect_Click(object sender, RoutedEventArgs e)
        {
            MenuItem item = sender as MenuItem;
            if (item == null)
                throw new NotImplementedException();
            string header = item.Header as string;
            if (string.IsNullOrEmpty(header))
                throw new NotImplementedException();

            if (this.worker.IsBusy) return;
            this.Title = this.Name + ": connecting to " + header;
            this.Filename = header;
            this.worker.RunWorkerAsync(header);
        }


        private void Zoom_ValueChanged(object sender, RoutedPropertyChangedEventArgs<double> e)
        {
            if (this.mainCanvas == null)
                return; // Still processing the XAML
            Console.WriteLine("{0} pixels/sec", Math.Pow(2, Zoom.Value));
            SetZoom();
            SetScroll();
            ReRender();
        }

        private void XScrollBar_Scroll(object sender, System.Windows.Controls.Primitives.ScrollEventArgs e)
        {
            SetScroll();
            if (e.ScrollEventType != ScrollEventType.ThumbTrack)
            {
                ReRender();
            }
        }

        private void zoomOut()
        {
            double targetPixelsPerSecond = this.dockPanel3.ActualWidth
                / (this.TimeRange / this.TicksPerSecond);
            double wish = Math.Floor(Math.Log(targetPixelsPerSecond, 2));
            Zoom.Value = Math.Max(Zoom.Minimum, wish);
            // Zoom_ValueChanged already called
        }

        private void zoomOut_Click(object sender, RoutedEventArgs e)
        {
            zoomOut();
        }

        private void Zoom_DragCompleted(object sender, DragCompletedEventArgs e)
        {
            SetZoom();
            SetScroll();
            ReRender();
        }

        private void dockPanel3_SizeChanged(object sender, SizeChangedEventArgs e)
        {
            if (e.WidthChanged)
            {
                SetZoom();
                SetScroll();
            }
            else if (e.HeightChanged)
            {
                this.mainCanvas.Height = e.NewSize.Height;
            }
            ReRender();
        }


        void BackgroundRender()
        {
            ReRender();
            ConsiderRescale();
        }

        // Functions for interacting with the background worker
        
        void worker_DoWork(object sender, DoWorkEventArgs e)
        {
            Console.WriteLine("worker_DoWork entered");

            if (this.worker.CancellationPending)
            {
                Console.WriteLine("worker_DoWork: cancelling...");
                e.Result = null;
                e.Cancel = true;
                return;
            }

            string target = e.Argument as string;

            if (target != null)
            {
                // Connecting to a new place
                if (this.TraceFetcher != null)
                    this.TraceFetcher.Dispose();
                try
                {
                    this.TraceFetcher = new TraceFetcher(target);
                    Console.WriteLine("tracefetcher was constructed");
                    if (this.worker.CancellationPending)
                    {
                        e.Result = null;
                        e.Cancel = true;
                        return;
                    }
                    this.worker.ReportProgress(1);
                }
                catch (Exception ex)
                {
                    if (this.worker.CancellationPending)
                    {
                        e.Result = null;
                        e.Cancel = true;
                        return;
                    }
                    e.Result = ex;
                    return;
                }
                this.TraceFetcher.Pipelining = this.Automatic;
            }

            MemoryStream ms = this.TraceFetcher.Next();
            if (this.worker.CancellationPending)
                e.Cancel = true;
            else if (ms != null)
                e.Result = new StreamReader(ms);
            else if (this.TraceFetcher != null)
                e.Result = this.TraceFetcher.Error;
            else
                throw new NotImplementedException("This shouldnt happen");

            Console.WriteLine("worker_DoWork exiting");
        } // worker_DoWork

        void worker_RunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            if (e.Cancelled) {
                Console.WriteLine("RunWorkerCompleted: worker cancelled!");
                this.RunCancel.Content = "Run";
                return;
            }
            StreamReader reader = e.Result as StreamReader;
            Exception ex;
            if (reader != null)
            {
                ReadDataFromStream(reader);
                reader.Close();
                // NOTA BENE: This is a *synchronous* Invoke, i.e. the BackgroundRender
                // will happen on the UI thread (and first any other UI) before this call
                // returns.  This includes e.g. "Disconnect" UI pending.  Therefore
                // think very carefully about the state machine!
                Dispatcher.Invoke(DispatcherPriority.Background,
                                  new Action(BackgroundRender));

                // Start fetching the next one
                if (this.TraceFetcher != null && this.Automatic && !this.worker.IsBusy)
                    this.worker.RunWorkerAsync(null);
            }
            else if ((ex = e.Result as Exception) != null)
            {
                MessageBox.Show(ex.ToString(), ex.Message, MessageBoxButton.OK, MessageBoxImage.Error);
                this.Title = this.Name + ": connection closed";
                this.Automatic = false;
                this.RunCancel.Content = "Run";
                if (this.TraceFetcher != null)
                    this.TraceFetcher.Dispose();
                this.TraceFetcher = null;
                this.mainCanvas.Children.Clear();
            }
            // else XXX

        }


        void worker_ProgressChanged(object sender, ProgressChangedEventArgs e)
        {
            if (e.ProgressPercentage == 1)
                this.Title = this.Name + ": connected to " + this.Filename;
            Console.WriteLine("Progress changed");
        }



        private void RunCancel_Click(object sender, RoutedEventArgs e)
        {
            if (this.TraceFetcher == null)
            {
                MessageBox.Show("Cannot fetch a trace when not connected", "Not connected", MessageBoxButton.OK, MessageBoxImage.Stop);
                return;
            }

            this.Automatic = !this.Automatic;
            this.TraceFetcher.Pipelining = this.Automatic;

            if (this.Automatic)
            {
                this.RunCancel.Content = "Stop";
                if (!this.worker.IsBusy)
                    this.worker.RunWorkerAsync(null);
            }
            else
            {
                this.RunCancel.Content = "Run";
                // XXX this.worker.Cancel();
                Console.WriteLine("User clicked Cancel");
                this.worker.CancelAsync();
            }
        } // RunCancel_Click

        private void Single_Click(object sender, RoutedEventArgs e)
        {
            this.mainCanvas.Children.Clear();
            if (this.TraceFetcher == null)
            {
                MessageBox.Show("Cannot fetch a trace when not connected", "Not connected", MessageBoxButton.OK, MessageBoxImage.Stop);
                return;
            }
            if (!this.worker.IsBusy)
                this.worker.RunWorkerAsync(null);
        }

        private void FileSave_Click(object sender, RoutedEventArgs e)
        {
            if (this.ttec == null)
                return;

            string filename;
            if (!Dialogs.SaveThese("bfl", "Barrelfish Log files|*.bfl", out filename))
                return;

            StreamWriter writer = new StreamWriter(filename);
            if (this.procnames != null)
                foreach (KeyValuePair<ulong, string> kvp in this.procnames)
                    writer.WriteLine("# DCB 0 {0} {1}", kvp.Key, kvp.Value);
            foreach (TemporalTraceEvent tte in this.ttec)
                writer.WriteLine(tte.ToString());
            writer.Close();
        } // FileSave_Click


        private void Disconnect_Click(object sender, RoutedEventArgs e)
        {
            this.Title = this.Name;

            this.worker.CancelAsync();
            if (this.TraceFetcher != null)
                this.TraceFetcher.Dispose();
            this.TraceFetcher = null;
            this.ttec = null;
            this.procnames = null;
            this.Filename = null;
            this.ReRender();
        }

        private void ViewKey_Click(object sender, RoutedEventArgs e)
        {
            Key x = new Key();
            x.Show();
        }

        private void Cores_Click(object sender, RoutedEventArgs e)
        {
            this.Cores4.IsChecked = (sender == this.Cores4);
            this.Cores8.IsChecked = (sender == this.Cores8);
            this.Cores16.IsChecked = (sender == this.Cores16);
            this.Cores24.IsChecked = (sender == this.Cores24);
            this.Cores32.IsChecked = (sender == this.Cores32);
            this.Cores64.IsChecked = (sender == this.Cores64);
            if (sender == this.Cores4)
                this.NumCores = 4;
            else if (sender == this.Cores8)
                this.NumCores = 8;
            else if (sender == this.Cores16)
                this.NumCores = 16;
            else if (sender == this.Cores24)
                this.NumCores = 24;
            else if (sender == this.Cores32)
                this.NumCores = 32;
            else if (sender == this.Cores64)
                this.NumCores = 64;
            this.ReRender();
        }
    } // class Window1



    public class TraceFetcher : IDisposable
    {
        // These must match the equivalent code in Barrelfish and Aquarium.
        /// <summary>
        /// The number of bytes of header containing the size of the trace
        /// </summary>
        const int HeaderSize = 8;
        /// <summary>
        /// The format string for converting an integer to a trace header
        /// </summary>
        const string HeaderFormatString = "00000000";


        public readonly string Target;
        public bool Pipelining;
        public Exception Error { get; private set; }

        private readonly byte[] tosend;
        private NetworkStream stream;
        private bool sentRequest;

        public TraceFetcher(string target)
        {
            int port = 666;
            string[] addrport = target.Split(':');
            if (addrport.Length == 2)
            {
                target = addrport[0];
                port = int.Parse(addrport[1]);
                Console.WriteLine("Connecting to {0} on non-default port {1}", target, port);
            }

            this.Target = target;
            this.tosend = Encoding.ASCII.GetBytes("trace\n");

            // The name could resolve to both v4 and v6 addressesw
            // but we know that barrelfish will only be listening on v4.
            // The test trace replay (testaquarium) also listens only on v4
            // so here we need to filter the addresses to avoid long (e.g. 
            // 30 second) hangs when connecting to a testaquarium.

            IPAddress[] addresses = Dns.GetHostAddresses(target);
            List<IPAddress> v4s = new List<IPAddress>(addresses.Length);
            for (int i = 0; i < addresses.Length; i++)
                if (addresses[i].AddressFamily == AddressFamily.InterNetwork)
                    v4s.Add(addresses[i]);

            TcpClient client = new TcpClient(AddressFamily.InterNetwork);
            client.Connect(v4s.ToArray(), port);
            this.stream = client.GetStream();
        }

        public void Dispose()
        {
            if (this.stream == null)
                return;
            this.stream.Close();
            this.stream = null;
        }

        /// <summary>
        /// Returns MemoryStream of the next trace.  If there is an error
        /// returns null, disposes itself, and sets Error property to the exception
        /// </summary>
        /// <returns>MemoryStream or null</returns>
        public MemoryStream Next()
        {
            if (this.stream == null)
                throw new ObjectDisposedException("TraceFetcher");

            try
            {
                if (this.sentRequest)
                    this.sentRequest = false;
                else
                    this.stream.Write(tosend, 0, tosend.Length);

                byte[] header = Read(HeaderSize);

                string lenstring = Encoding.ASCII.GetString(header);
                if (lenstring.Length != HeaderSize)
                    throw new FormatException("header changed size after conversion: " + lenstring);
                foreach (char c in lenstring)
                    if (c < '0' || c > '9')
                        throw new FormatException("header contains non digits: " + lenstring);
                int len;
                if (!int.TryParse(lenstring, out len))
                    throw new FormatException("Not a valid length: " + lenstring);

                byte[] data = Read(len);
                MemoryStream ms = new MemoryStream(data);
                if (this.Pipelining)
                {
                    this.stream.Write(tosend, 0, tosend.Length);
                    this.sentRequest = true;
                }

                return ms;
            }
            catch (Exception ex)
            {
                this.Error = ex;
                this.Dispose();
                return null;
            }
        } // Next


        /// <summary>
        /// Keeps reading until it gets all of the bytes requested or fails
        /// with an EndOfStreamException
        /// </summary>
        /// <param name="len">Number of bytes to read</param>
        /// <returns>The bytes read</returns>
        private byte[] Read(int len)
        {
            byte[] data = new byte[len];
            int position = 0;
            do
            {
                int got = stream.Read(data, position, data.Length - position);
                if (got == 0)
                    throw new EndOfStreamException();
                position += got;
            } while (position < data.Length);
            return data;
        }
    } // class TraceFetcher

    #region WPF helper classes

    public class VisualHost : FrameworkElement
    {
        private VisualCollection _children;
        public VisualHost()
        {
            _children = new VisualCollection(this);
        }

        public void AddVisual(DrawingVisual vis)
        {
            _children.Add(vis);
        }
        // Provide a required override for the VisualChildrenCount property.
        protected override int VisualChildrenCount
        {
            get { return _children.Count; }
        }
        // Provide a required override for the GetVisualChild method.
        protected override Visual GetVisualChild(int index)
        {
            if (index < 0 || index > _children.Count)
            {
                throw new ArgumentOutOfRangeException();
            }

            return _children[index];
        }
    } // class VisualHost


    public class MyDrawingVisual : DrawingVisual
    {
        public DrawingContext dc;

        public MyDrawingVisual()
        {
        }
        public void BeginRender()
        {
            this.dc = this.RenderOpen();
        }
        public void EndRender()
        {
            this.dc.Close();
        }

    } // class MyDrawingVisual


    public class Highlighting
    {
        private readonly List<Shape> oldshapes;
        private readonly List<TextBlock> oldblocks;
        private readonly List<Brush> oldstroke, oldfill, oldforeground;

        private readonly SolidColorBrush brush;

        public Highlighting(SolidColorBrush brush)
        {
            this.brush = brush.Clone();
            this.brush.Freeze();
            this.oldshapes = new List<Shape>();
            this.oldblocks = new List<TextBlock>();
            this.oldstroke = new List<Brush>();
            this.oldfill = new List<Brush>();
            this.oldforeground = new List<Brush>();
        }

        public void Set(params FrameworkElement[] elements)
        {
            if (oldshapes.Count > 0 || oldblocks.Count > 0)
                Clear();
            if (elements == null || elements.Length == 0)
                return;
            Shape shape;
            TextBlock block;
            for (int i = 0; i < elements.Length; i++)
            {
                if ((shape = elements[i] as Shape) != null)
                {
                    oldshapes.Add(shape);
                    oldstroke.Add(shape.Stroke);
                    oldfill.Add(shape.Fill);

                    shape.Stroke = brush;
                    if (shape.Fill != null)
                    {
                        SolidColorBrush inside = new SolidColorBrush(brush.Color);
                        inside.Opacity = shape.Fill.Opacity;
                        shape.Fill = inside;
                    }
                }
                else if ((block = elements[i] as TextBlock) != null)
                {
                    oldblocks.Add(block);
                    oldforeground.Add(block.Foreground);
                    block.Foreground = this.brush;
                }
            }
        }

        public void Clear()
        {
            for (int i = 0; i < oldshapes.Count; i++)
            {
                oldshapes[i].Stroke = oldstroke[i];
                oldshapes[i].Fill = oldfill[i];
            }
            oldshapes.Clear();
            oldstroke.Clear();
            oldfill.Clear();

            for (int i = 0; i < oldblocks.Count; i++)
                oldblocks[i].Foreground = oldforeground[i];
            oldforeground.Clear();
            oldblocks.Clear();
        }
    } // class Highlighting

    #endregion

} // namespace
