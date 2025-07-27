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

import java.io.IOException;
import java.util._;

import ghidra.app.util.Option;
import ghidra.app.util.bin.ByteProvider;
import ghidra.app.util.importer.MessageLog;
import ghidra.app.util.opinion.AbstractLibrarySupportLoader;
import ghidra.app.util.opinion.LoadSpec;
import ghidra.framework.model.DomainObject;
import ghidra.program.model.listing.Program;
import ghidra.util.exception.CancelledException;
import ghidra.util.task.TaskMonitor;

/** TODO: Provide class-level documentation that describes what this loader
  * does.
  */
class SkeletonLoader extends AbstractLibrarySupportLoader {

  override def getName(): String = {

    // TODO: Name the loader.  This name must match the name of the loader in the .opinion
    // files.

    "My loader"
  }

  @throws(classOf[IOException])
  override def findSupportedLoadSpecs(
      provider: ByteProvider
  ): Collection[LoadSpec] = {
    val loadSpecs: ArrayList[LoadSpec] = new ArrayList();

    // TODO: Examine the bytes in 'provider' to determine if this loader can load it.  If it
    // can load it, return the appropriate load specifications.

    loadSpecs
  }

  // throws CancelledException, IOException
  override def load(
      provider: ByteProvider,
      loadSpec: LoadSpec,
      options: List[Option],
      program: Program,
      monitor: TaskMonitor,
      log: MessageLog
  ): Unit = {

    // TODO: Load the bytes from 'provider' into the 'program'.
  }

  override def getDefaultOptions(
      provider: ByteProvider,
      loadSpec: LoadSpec,
      domainObject: DomainObject,
      isLoadIntoProgram: Boolean
  ): List[Option] = {
    val list =
      super.getDefaultOptions(
        provider,
        loadSpec,
        domainObject,
        isLoadIntoProgram
      );

    // TODO: If this loader has custom options, add them to 'list'
    list.add(
      new Option("Option name goes here", "Default option value goes here")
    );

    return list;
  }

  override def validateOptions(
      provider: ByteProvider,
      loadSpec: LoadSpec,
      options: List[Option],
      program: Program
  ): String = {

    // TODO: If this loader has custom options, validate them here.  Not all options require
    // validation.

    super.validateOptions(provider, loadSpec, options, program)
  }
}
