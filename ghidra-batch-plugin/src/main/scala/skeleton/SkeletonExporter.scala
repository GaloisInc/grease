/* ###
 * IP: GHIDRA
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package skeleton;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import ghidra.app.util._;
import ghidra.app.util.exporter.Exporter;
import ghidra.app.util.exporter.ExporterException;
import ghidra.framework.model.DomainObject;
import ghidra.program.model.address.AddressSetView;
import ghidra.util.task.TaskMonitor;

/** TODO: Provide class-level documentation that describes what this exporter
  * does.
  */
class SkeletonExporter extends Exporter("My Exporter", "exp", null) {

  // throws ExporterException, IOException
  override def `export`(
      file: File,
      domainObj: DomainObject,
      addrSet: AddressSetView,
      monitor: TaskMonitor
  ): Boolean = {

    // TODO: Perform the export, and return true if it succeeded

    false
  }

  override def getOptions(
      domainObjectService: DomainObjectService
  ): List[Option] = {
    var list: List[Option] = new ArrayList();

    // TODO: If this exporter has custom options, add them to 'list'
    list.add(
      new Option("Option name goes here", "Default option value goes here")
    );

    list
  }

  @throws(classOf[OptionException])
  override def setOptions(
      options: List[Option]
  ): Unit = {

    // TODO: If this exporter has custom options, assign their values to the exporter here
  }
}
