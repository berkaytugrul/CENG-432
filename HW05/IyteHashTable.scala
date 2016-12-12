class IyteHashTable {

  private var capacity = 1000000;
  private var threshold : Int = (0.7f * capacity).asInstanceOf[Int];
  private var size : Int = 0;

  private var table : Array[IyteStringEntry] = new Array[IyteStringEntry](capacity);

  private class IyteStringEntry(val hash: Int, val key: String, var value: String, var next: IyteStringEntry);

  def set(key: String, value: String): Unit = {
    val h = if(key != null && key != "") hash(key) else hash("null");
    val index = indexFor(h);

    var p = table(index);

    var keyExists: Boolean = false;

    if(p == null){
      table(index) = new IyteStringEntry(h,key,value,null);
    }else{

      var ended : Boolean = false;

      while(!keyExists && !ended){
        if(p.hash == h && p.key == key){
          keyExists = true;
        }else if(p.next != null){
          p = p.next;
        }else{
          ended = true;
        }
      }

      if(keyExists){
        p.value = value
      }else{
        p.next = new IyteStringEntry(h,key,value, null);
      }
    }

    if(!keyExists){
      size += 1;
    }

    if(size > threshold){
      this.resize();
    }

  }

  def get(key: String): String = {
    val h = if(key != null && key != "") hash(key) else hash("null");
    val index: Int = indexFor(h);

    var p: IyteStringEntry = table(index);

    var value: String = null;

    if(p != null){

      var keyFound: Boolean = false;

      do{
        if(p.hash == h){
          if(p.key == key){
            keyFound = true;
          }else{
            p = p.next;
          }
        }else{
          p = p.next;
        }
      }while(p != null && !keyFound);

      if(keyFound){
        value = p.value;
      }

    }

    value
  }

  def getSize(): Int ={
    this.size
  }

  private def resize(): Unit ={
    val oldTable = table;
    val oldCapacity = capacity;

    capacity *= 2;
    table = new Array[IyteStringEntry](capacity);
    threshold = (capacity * 0.7f).asInstanceOf[Int];

    for(i:Int <- 0 until oldCapacity){
      var entry = oldTable(i);

      if(entry != null){
        do{
          val next = putEntryToEmptyTable(entry);
          entry = next;
        }while(entry != null);
      }
    }
  }

  private def putEntryToEmptyTable(entry: IyteStringEntry): IyteStringEntry = {
    val index = indexFor(entry.hash);
    var p = table(index);

    if(p == null){
      table(index) = entry;
    }else{

      while(p.next != null){
        p = p.next
      }
      p.next = entry;
    }

    val oldNextEntry = entry.next;
    entry.next = null;

    oldNextEntry
  }

  private def indexFor(hash: Int): Int ={
    val index = hash % capacity;
    if(index < 0){
      index + capacity
    }else{
      index
    }
  }

  private final def hash(str: String): Int = {

    var h = 0xf7ca7fd2
    var i = 0
    while (i + 1 < str.length) {
      val data = (str.charAt(i) << 16) + str.charAt(i + 1)
      h = mixLast(h, data)
      h = java.lang.Integer.rotateLeft(h, 13)
      h = h * 5 + 0xe6546b64
      i += 2
    }

    if (i < str.length) h = mixLast(h, str.charAt(i).toInt)

    h ^= str.length
    h ^= h >>> 16
    h *= 0x85ebca6b
    h ^= h >>> 13
    h *= 0xc2b2ae35
    h ^= h >>> 16

    h
  }
  private final def mixLast(hash: Int, data: Int): Int = {
    var k = data

    k *= 0xcc9e2d51
    k = java.lang.Integer.rotateLeft(k, 15)
    k *= 0x1b873593

    hash ^ k
  }

  def hist(): String ={

    val b = new StringBuilder();

    for(i:Int<-0 until table.length){
      var p = table(i);
      if(p != null){
        var i = 0;
        while(p != null){
          i += 1;
          p = p.next;
        }
        b.append(i);
      }else{
        b.append("0");
      }

      b.append("\n");
    }

    b.toString()
  }
}

object IyteHashTable{
  def apply() = new IyteHashTable();
}