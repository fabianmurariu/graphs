package com.github.fabianmurariu

import com.github.fabianmurariu.graphs.kernel.Graph
import com.github.fabianmurariu.graphs.data.dg.LookupTable
import com.github.fabianmurariu.graphs.data.dg.EntryIndex

package object graphs {
  object syntax
      extends Graph.ToGraphOps
      with LookupTable.ToLookupTableOps
      with EntryIndex.ToEntryIndexOps
}
