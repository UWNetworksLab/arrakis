using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Globalization;
using System.Collections.ObjectModel;
using System.IO;

namespace Aquarium
{
    public struct TraceEvent
    {
        /// <summary>
        /// The full 64 bits of the event
        /// </summary>
        public readonly ulong Raw;

        /// <summary>
        /// The top 16 bits of the event - by convention the subsystem
        /// </summary>
        public ushort SubSys { get { return (ushort)(Raw >> 48); } }
        /// <summary>
        /// The next 16 bits of the event - by convention the event type
        /// </summary>
        public ushort Event { get { return (ushort)((Raw >> 32) & 0xFFFF); } }
        /// <summary>
        /// The bottom 32 bits of the event
        /// </summary>
        public uint Data { get { return (uint)(Raw & 0xFFFFFFFFUL); } }
        /// <summary>
        /// True if this is a Send event
        /// </summary>
        public bool IsSend { get { return ((Raw & (0xFFUL << 56)) == (0xEAUL << 56)); } }
        /// <summary>
        /// True if this is a Recv event
        /// </summary>
        public bool IsRecv { get { return ((Raw & (0xFFUL << 56)) == (0xEBUL << 56)); } }
        /// <summary>
        /// True if either a Send or Recv event
        /// </summary>
        public bool IsRxTx { get { return ((Raw & (0xFEUL << 56)) == (0xEAUL << 56)); } }
        /// <summary>
        /// The communication channel identifier
        /// </summary>
        public ulong Channel { get { return ((Raw & 0x00FFFFFFFF000000UL) >> 12); } }
        /// <summary>
        /// The message sequence number on the channel
        /// </summary>
        public uint Sequence { get { return (uint)(Raw & 0x00FFFFFF); } }

        public override string ToString()
        {
            if (IsRxTx)
                return String.Format("{0}/{1:x}/{2:x}",
                    IsRecv ? "Rx" : "Tx", Channel, Sequence);
            else
                return String.Format("{0:x}/{1:x}/{2:x}",
                    SubSys, Event, Data);
        }


        public TraceEvent(ulong raw)
        {
            this.Raw = raw;
        }

        public static bool TryParse(string s, out TraceEvent result)
        {
            ulong raw;
            bool ok = ulong.TryParse(s, NumberStyles.AllowHexSpecifier, null, out raw);
            result = ok ? new TraceEvent(raw) : new TraceEvent();
            return ok;
        }
    } // struct TraceEvent



    public struct TemporalTraceEvent
    {
        public readonly int Core;
        public readonly long Time;
        public readonly TraceEvent Event;

        public TemporalTraceEvent(int core, long time, TraceEvent e)
        {
            if (core < 0)
                throw new ArgumentOutOfRangeException("core");
            if (time < 0)
                throw new ArgumentOutOfRangeException("time");
            this.Core = core;
            this.Time = time;
            this.Event = e;
        }

        public override string ToString()
        {
            return Core.ToString() + Time.ToString() + Event.ToString();
        }


        public static bool TryParse(string core, string time, string args,
            out TemporalTraceEvent result)
        {
            result = default(TemporalTraceEvent);
            int c;
            long t;
            TraceEvent e;
            if (!int.TryParse(core, out c)
                || !long.TryParse(time, out t)
                || !TraceEvent.TryParse(args, out e))
                return false;
            result = new TemporalTraceEvent(c, t, e);
            return true;
        }

        public static bool TryParse(string s, out TemporalTraceEvent result)
        {
            string[] tokens = s.Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
            if (tokens.Length < 3)
            {
                result = default(TemporalTraceEvent);
                return false;
            }
            return TryParse(tokens[0], tokens[1], tokens[2], out result);
        }
    } // struct TemporalTraceEvent


    public class TemporalTraceEventCollection : Collection<TemporalTraceEvent>
    {
        public bool Add(string s)
        {
            TemporalTraceEvent tte;
            if (!TemporalTraceEvent.TryParse(s, out tte))
                return false;
            this.Add(tte);
            return true;
        }

        public bool Add(string core, string time, string args)
        {
            TemporalTraceEvent tte;
            if (!TemporalTraceEvent.TryParse(core, time, args, out tte))
                return false;
            this.Add(tte);
            return true;
        }

        private bool Calculated;

        protected override void InsertItem(int index, TemporalTraceEvent item)
        {
            base.InsertItem(index, item);
            this.Calculated = false;
        }

        protected override void RemoveItem(int index)
        {
            base.RemoveItem(index);
            this.Calculated = false;
        }

        protected override void SetItem(int index, TemporalTraceEvent item)
        {
            base.SetItem(index, item);
            this.Calculated = false;
        }

        protected override void ClearItems()
        {
            base.ClearItems();
            this.Calculated = false;
        }

        // Cached properties
        private int mincore, maxcore;
        private long mintime, maxtime;

        private void Calculate()
        {
            if (this.Count == 0)
                return;
            mincore = int.MaxValue;
            maxcore = int.MinValue;
            mintime = long.MaxValue;
            maxtime = long.MinValue;
            for (int i = 0; i < this.Count; i++)
            {
                int core = this[i].Core;
                long time = this[i].Time;
                if (mincore > core)
                    mincore = core;
                if (maxcore < core)
                    maxcore = core;

                if (mintime > time)
                    mintime = time;
                if (maxtime < time)
                    maxtime = time;
            }
            this.Calculated = true;
        }

        public int MinCore { get { if (!Calculated) Calculate(); return mincore; } }
        public int MaxCore { get { if (!Calculated) Calculate(); return maxcore; } }

        public long MinTime { get { if (!Calculated) Calculate(); return mintime; } }
        public long MaxTime { get { if (!Calculated) Calculate(); return maxtime; } }

        // Data sub-sets

        public IEnumerable<TemporalTraceEvent> OnCore(int core)
        {
            for (int i = 0; i < this.Count; i++)
                if (this[i].Core == core)
                    yield return this[i];
        }
    } // class TemporalTraceEventCollection
} // namespace
