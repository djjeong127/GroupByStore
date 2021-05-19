/**
 * GroupByStore.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: djjeong
 * Person#: 50270181
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa2

import cse250.objects.{DNode, TaxEntry}
import collection.mutable.ArrayBuffer
import util.control.Breaks._

class GroupByStore {
  // Feel free to change the default value of groupings and modify it to val/var.
  var groupings: ArrayBuffer[DNode[TaxEntry]] = new ArrayBuffer[DNode[TaxEntry]]
  private var groupingAttribute = "STREET"
  var numStored = 0

  /** Inserts element to head of corresponding grouping list. */
  def insert(taxEntry: TaxEntry): Unit = {
    var exists: Boolean = false
    var newnode: DNode[TaxEntry] = new DNode[TaxEntry](taxEntry, null, null)
    //insert beginning DNode if nothing in grouping
    if (groupings.isEmpty) {
      groupings.addOne(newnode)
    }
    //compare with current index, insert where its needed
    else {
      for (node <- groupings) {
        //if there exists a node with same group type
        if (node.value.infoMap(groupingAttribute) == taxEntry.infoMap(groupingAttribute)) {
          node.prev = newnode
          newnode.next = node
          groupings(groupings.indexOf(node)) = newnode
          exists = true
        }
      }
      //if node group doesn't exist
      if (!exists) {
        //extend array length by one
        groupings.addOne(new DNode[TaxEntry](taxEntry, null, null))
        var hold: Int = 0
        for (node <- groupings) {
          if (hold == 0 && taxEntry.infoMap(groupingAttribute) < node.value.infoMap(groupingAttribute)) {
            val nodeindex: Int = groupings.indexOf(node)
            var index: Int = groupings.length - 1
            //starting from tail index - 1, shift over one to the right
            while (index != nodeindex) {
              groupings(index) = groupings(index - 1)
              index -= 1
            }
            //insert node where theres now a space
            groupings(nodeindex) = newnode
            hold += 1
          }
        }
      }
    }
    //update numStored
    numStored += 1
  }


  /** Regroup . */
  def regroup(attribute: String): Unit = {
    //if attributes are equal do nothing
    if (attribute == groupingAttribute) {
      ;
    }
    //if attributes are different
    else {
      val newgroupings: ArrayBuffer[DNode[TaxEntry]] = groupings
      groupings = new ArrayBuffer[DNode[TaxEntry]]
      groupingAttribute = attribute
      for (headnode <- newgroupings) {
        var node: DNode[TaxEntry] = new DNode[TaxEntry](headnode.value, headnode.prev, headnode.next)
        while (node != null) {
          insert(node.value)
          node = node.next
        }
      }
    }

  }

  /** Returns an Iterator to all entries that can be used only once. */
  def iterator: Iterator[TaxEntry] = new Iterator[TaxEntry] {
    var index: Int = 0
    var currentheadNode: DNode[TaxEntry] = null


    override def hasNext: Boolean = {
      //if starting from groupings(0) or ended a list and ready to move on to next index
      if (currentheadNode == null && numStored > 0  && index < groupings.length) {
        currentheadNode = groupings(index)
        index += 1
        currentheadNode != null
      }
      else {
        currentheadNode != null
      }
    }

    override def next(): TaxEntry = {
      val currententry: TaxEntry = currentheadNode.value
      currentheadNode = currentheadNode.next
      currententry
    }
  }
  
  /** Returns an Iterator to only the entries with matching values on the grouping attribute that can be used only once. */
  def iterator(value: String): Iterator[TaxEntry] = new Iterator[TaxEntry] {
    var exists: Boolean = false
    var index: Int = 0
    var currentheadNode: DNode[TaxEntry] = null
    if (numStored > 0) {
      for (node <- groupings) {
        if (node.value.infoMap(groupingAttribute) == value) {
          index = groupings.indexOf(node)
          exists = true
        }
      }
    }

    override def hasNext: Boolean = {
      if (currentheadNode == null && exists) {
        currentheadNode = groupings(index)
        exists = false
        currentheadNode != null
      }
      else {
        currentheadNode != null
      }
    }

    override def next(): TaxEntry = {
      var currententry: TaxEntry = currentheadNode.value
      currentheadNode = currentheadNode.next
      currententry
    }
  }

  def length: Int = numStored

  override def toString: String = if (numStored == 0) "" else this.iterator.addString(new StringBuilder, "\n").result()
}